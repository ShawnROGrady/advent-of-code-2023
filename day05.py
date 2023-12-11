import dataclasses
import typing
import argparse


def flattened_options(options):
    for option in options:
        if option is None:
            continue
        for x in option:
            yield x


@dataclasses.dataclass(frozen=True)
class Range:
    start: int
    end: int

    @classmethod
    def from_start(cls, start: int, length: int) -> "Range":
        return cls(start, start + length)

    def __contains__(self, item) -> bool:
        return item >= self.start and item < self.end

    def __len__(self) -> int:
        return self.end - self.start

    def inter(self, other: "Range") -> typing.Optional["Range"]:
        if self.end <= other.start or self.start >= other.end:
            return None

        start, end = max((self.start, other.start)), min((self.end, other.end))
        return Range(start, end)

    def _sub_one(self, other: "Range") -> typing.Optional[typing.Iterable["Range"]]:
        if self.end <= other.start or self.start >= other.end:
            return (self,)

        if self.start < other.start:
            leading = Range(self.start, other.start)
            if self.end <= other.end:
                return (leading,)

            trailing = Range(other.end, self.end)
            return (leading, trailing)
        if self.end > other.end:
            trailing = Range(other.end, self.end)
            return (trailing,)

        return None

    def sub(self, *others: "Range") -> typing.Optional[typing.Iterable["Range"]]:
        cur = (self,)
        for other in others:
            subbed = map(lambda x: x._sub_one(other), cur)

            cur = tuple(flattened_options(subbed))
            if len(cur) == 0:
                return None
        return cur


@dataclasses.dataclass(frozen=True)
class RangeMap:
    dst_start: int
    src_start: int
    length: int

    @property
    def dst_range(self) -> Range:
        return Range.from_start(self.dst_start, self.length)

    @property
    def src_range(self) -> Range:
        return Range.from_start(self.src_start, self.length)

    @property
    def dst_end(self) -> int:
        return self.dst_range.end

    @property
    def src_end(self) -> int:
        return self.src_range.end

    @property
    def dst_delta(self) -> int:
        return self.dst_start - self.src_start

    def __contains__(self, item: int) -> bool:
        return item in self.src_range

    def __getitem__(self, key: int) -> int:
        if key in self:
            return key + self.dst_delta
        raise KeyError(key)


@dataclasses.dataclass(frozen=True)
class Mapping:
    segments: typing.List[RangeMap]

    def get(self, key: int) -> int:
        for seg in self.segments:
            if key not in seg:
                continue
            return seg[key]

        return key

    def __contains__(self, item: int) -> bool:
        return any(map(lambda seg: item in seg, self.segments))

    def __call__(self, item: int) -> int:
        return self.get(item)

    def join(self, other: "Mapping") -> "Mapping":
        overlapping = []

        for my_seg in self.segments:
            for other_seg in other.segments:
                inter = my_seg.dst_range.inter(other_seg.src_range)
                if inter is None:
                    continue
                overlapping.append(
                    RangeMap(
                        src_start=inter.start - my_seg.dst_delta,
                        dst_start=inter.start + other_seg.dst_delta,
                        length=len(inter),
                    )
                )

        mine_only = []
        other_ranges = [seg.src_range for seg in other.segments]
        for my_seg in self.segments:
            subbed = my_seg.dst_range.sub(*other_ranges)
            if subbed is None:
                continue
            mine_only.extend(
                [
                    RangeMap(
                        src_start=s.start - my_seg.dst_delta,
                        dst_start=s.start,
                        length=len(s),
                    )
                    for s in subbed
                ]
            )

        other_only = []
        my_ranges = [seg.dst_range for seg in self.segments]
        for other_seg in other.segments:
            subbed = other_seg.src_range.sub(*my_ranges)
            if subbed is None:
                continue
            other_only.extend(
                [
                    RangeMap(
                        src_start=s.start,
                        dst_start=other_seg.dst_start + (s.start - other_seg.src_start),
                        length=len(s),
                    )
                    for s in subbed
                ]
            )

        return Mapping(overlapping + mine_only + other_only)


@dataclasses.dataclass(frozen=True)
class Almanac:
    seeds: typing.List[int]

    seed_to_soil: Mapping
    soil_to_fertilizer: Mapping
    fertilizer_to_water: Mapping
    water_to_light: Mapping
    light_to_temperature: Mapping
    temperature_to_humidity: Mapping
    humidity_to_location: Mapping

    @property
    def seed_to_location(self) -> RangeMap:
        return self.seed_to_soil.join(
            self.soil_to_fertilizer.join(
                self.fertilizer_to_water.join(
                    self.water_to_light.join(
                        self.light_to_temperature.join(
                            self.temperature_to_humidity.join(self.humidity_to_location)
                        )
                    )
                )
            )
        )

    def location_of_seed(self, seed: int) -> int:
        return self.humidity_to_location(
            self.temperature_to_humidity(
                self.light_to_temperature(
                    self.water_to_light(
                        self.fertilizer_to_water(
                            self.soil_to_fertilizer(self.seed_to_soil(seed))
                        )
                    )
                )
            )
        )


def parse_range_map(s: str) -> RangeMap:
    dst_start, src_start, length = tuple(map(int, s.split()))
    return RangeMap(dst_start, src_start, length)


def parse_seeds(s: str) -> typing.List[int]:
    fst, snd = s.split(": ")
    if fst != "seeds":
        raise ValueError(s)

    return [int(part) for part in snd.split()]


def parse_almanac(lines: typing.List[str]):
    seed_line, _, *lines = lines

    vals = {"seeds": parse_seeds(seed_line)}
    section = ""
    cur_vals = []

    for line in map(str.strip, lines):
        if line == "":
            vals[section] = Mapping(cur_vals)
            cur_vals = []
            section = ""
            continue

        if line.endswith("map:"):
            fst, _ = line.split()
            section = fst.replace("-", "_")
            continue

        cur_vals.append(parse_range_map(line))
    if section != "" and len(cur_vals) != 0:
        vals[section] = Mapping(cur_vals)

    return Almanac(**vals)


class Part1:
    @staticmethod
    def run(input: typing.List[str]) -> int:
        almanac = parse_almanac(input)
        seed_to_loc = almanac.seed_to_location
        locs = map(seed_to_loc, almanac.seeds)
        return min(locs)


def list_to_pairs(l):
    for i in range(0, int(len(l) / 2)):
        yield (l[2 * i], l[2 * i + 1])


class Part2:
    @staticmethod
    def run(input: typing.List[str]) -> int:
        almanac = parse_almanac(input)
        seed_to_loc = almanac.seed_to_location
        seed_ranges = list(
            map(lambda pair: Range.from_start(*pair), list_to_pairs(almanac.seeds))
        )

        def seed_in_almanac(seed: int) -> bool:
            return any(map(lambda r: seed in r, seed_ranges))

        src_starts = map(lambda seg: seg.src_start, seed_to_loc.segments)
        points_of_interest = filter(seed_in_almanac, src_starts)

        return min(map(seed_to_loc, points_of_interest))


def main(argv: typing.Optional[typing.List[str]] = None):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-part",
        dest="part",
    )
    parser.add_argument("in_file", metavar="IN_FILE")

    args = parser.parse_args(argv)

    with open(args.in_file, "r", encoding="utf-8") as f:
        lines = f.readlines()

    if args.part == "1":
        print(Part1.run(lines))
        return

    if args.part == "2":
        print(Part2.run(lines))
        return

    r1, r2 = Part1.run(lines), Part2.run(lines)
    print(f"part1:\n{r1}\npart2:\n{r2}")


if __name__ == "__main__":
    main()
