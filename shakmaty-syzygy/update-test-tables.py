#!/usr/bin/python

import chess
import chess.variant
import chess.syzygy


def update_dependencies(variant, suite, target):
    tables = set()

    for i, line in enumerate(open(suite)):
        if i == 0:
            continue

        epd, wdl, dtz = line.split(",")
        board, _ = chess.variant.find_variant(variant).from_epd(epd)

        table = chess.syzygy.calc_key(board)
        w, b = table.split("v", 1)
        if w and b:
            tables.add(table)

    one_king = variant != "antichess"
    tables = list(chess.syzygy.all_dependencies(tables, one_king))
    tables.sort()
    tables.sort(key=len)

    with open(target, "w") as out:
        for table in tables:
            base = "https://tablebase.lichess.ovh/tables/{}".format(variant)

            if variant == "standard":
                if len(table) <= 6:
                    print("{}/3-4-5/{}.rtbw".format(base, table), file=out)
                    print("{}/3-4-5/{}.rtbz".format(base, table), file=out)
                else:
                    print("{}/6-wdl/{}.rtbw".format(base, table), file=out)
                    print("{}/6-dtz/{}.rtbz".format(base, table), file=out)
            elif variant == "atomic":
                if len(table) <= 6:
                    print("{}/3-4-5/{}.atbw".format(base, table), file=out)
                    print("{}/3-4-5/{}.atbz".format(base, table), file=out)
                else:
                    print("{}/6-wdl/{}.atbw".format(base, table), file=out)
                    print("{}/6-dtz/{}.atbz".format(base, table), file=out)
            elif variant == "antichess":
                suffix = "giveaway" if "P" in table else "pawnless"
                ext = "gtb" if "P" in table else "stb"

                if len(table) <= 6:
                    print("{}/2-3-4-5-{}/{}.{}w".format(base, suffix, table, ext), file=out)
                    print("{}/2-3-4-5-{}/{}.{}z".format(base, suffix, table, ext), file=out)
                else:
                    print("{}/6-{}/{}.{}w".format(base, suffix, table, ext), file=out)
                    print("{}/6-{}/{}.{}z".format(base, suffix, table, ext), file=out)


if __name__ == "__main__":
    update_dependencies("standard", "tests/regular.csv", "tables/regular/TEST-SOURCE.txt")
    update_dependencies("atomic", "tests/atomic.csv", "tables/atomic/TEST-SOURCE.txt")
    update_dependencies("antichess", "tests/giveaway.csv", "tables/giveaway/TEST-SOURCE.txt")
