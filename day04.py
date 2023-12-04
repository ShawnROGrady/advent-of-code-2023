import typing
import dataclasses
import functools
import operator
import argparse
import itertools


@dataclasses.dataclass(frozen=True)
class Card:
    num: int
    winning_nums: typing.List[str]
    actual_nums: typing.List[str]

    def matching_nums(self) -> typing.FrozenSet[str]:
        return frozenset(self.winning_nums) & frozenset(self.actual_nums)


def _parse_nums(s: str) -> typing.Iterable[int]:
    return map(int, s.split())


def parse_card(line):
    card_def, to_parse = tuple(line.split(": "))
    card_num = int(card_def.split()[1])
    winning_str, actual_str = to_parse.split(" | ")
    return Card(
        card_num,
        winning_nums=list(_parse_nums(winning_str)),
        actual_nums=list(_parse_nums(actual_str)),
    )


class Part1:
    @staticmethod
    def score_from_matches(num_matches: int) -> int:
        if num_matches == 0 or num_matches == 1:
            return num_matches

        return 2 ** (num_matches - 1)

    @staticmethod
    def score_one(card: Card) -> int:
        num_matches = len(card.matching_nums())
        return Part1.score_from_matches(num_matches)

    @staticmethod
    def run(input: typing.Iterable[str]) -> int:
        cards = map(parse_card, input)

        scores = map(Part1.score_one, cards)
        return functools.reduce(operator.add, scores, 0)


class CardDuplicator:
    def __init__(self, cards: typing.Iterable[Card]):
        self.cached: typing.Dict[int, int] = {}
        self.num_matches_by_card = {
            card.num: len(card.matching_nums()) for card in cards
        }

    def produced_dupes(self, card_num: int) -> int:
        try:
            return self.cached[card_num]
        except KeyError:
            pass

        count = self.num_matches_by_card.get(card_num, 0)
        if count == 0:
            self.cached[card_num] = 0
            return 0

        extra = count
        for n in range(card_num + 1, card_num + count + 1):
            extra += self.produced_dupes(n)

        self.cached[card_num] = extra
        return extra


class Part2:
    @staticmethod
    def run(input: typing.Iterable[str]) -> int:
        cards = list(map(parse_card, input))

        duper = CardDuplicator(cards)
        scores = map(
            lambda card: duper.produced_dupes(card.num) + 1,
            cards,
        )
        return functools.reduce(operator.add, scores, 0)


def line_gen(io: typing.IO[str]) -> typing.Iterable[str]:
    while True:
        try:
            line = io.readline()
            if len(line) == 0:
                return
            yield line
        except EOFError:
            return


def main(argv: typing.Optional[typing.List[str]] = None):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--part", dest="part", choices=["1", "2", "both"], default="both"
    )
    parser.add_argument("in_file")

    args = parser.parse_args(argv)

    with open(args.in_file, "r", encoding="utf8") as f:
        lines = line_gen(f)

        if args.part == "1":
            print(Part1.run(lines))
            return

        if args.part == "2":
            print(Part2.run(lines))
            return

        fst, snd = itertools.tee(lines)
        r1, r2 = Part1.run(fst), Part2.run(snd)
        print(f"part1:\n{r1}\npart2:\n{r2}")


if __name__ == "__main__":
    main()