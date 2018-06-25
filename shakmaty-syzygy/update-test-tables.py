#!/usr/bin/python

import chess
import chess.variant
import chess.syzygy


def update_dependencies(variant, suite, target):
    dtz_tables = set()

    for i, line in enumerate(open(suite)):
        if i == 0:
            continue

        epd, wdl, dtz = line.split(",")
        board, _ = chess.variant.find_variant(variant).from_epd(epd)

        table = chess.syzygy.normalize_tablename(chess.syzygy.calc_key(board))
        w, b = table.split("v", 1)
        if w and b:
            dtz_tables.add(table)

    one_king = variant != "antichess"
    wdl_tables = list(chess.syzygy.all_dependencies(dtz_tables, one_king))
    wdl_tables.sort()
    wdl_tables.sort(key=len)

    with open(target, "w") as out:
        for table in wdl_tables:
            w, b = table.split("v")
            also_dtz = table in dtz_tables
            base = "https://tablebase.lichess.ovh/tables/{}".format(variant)

            if variant == "standard":
                if len(table) <= 6:
                    print("{}/3-4-5/{}.rtbw".format(base, table), file=out)
                    if also_dtz:
                        print("{}/3-4-5/{}.rtbz".format(base, table), file=out)
                elif len(table) <= 7:
                    print("{}/6-wdl/{}.rtbw".format(base, table), file=out)
                    if also_dtz:
                        print("{}/6-dtz/{}.rtbz".format(base, table), file=out)
                else:
                    suffix = "pawnful" if "P" in table else "pawnless"
                    print("{}/7/{}v{}_{}/{}.rtbw".format(base, len(w), len(b), suffix, table), file=out)
                    if also_dtz:
                        print("{}/7/{}v{}_{}/{}.rtbz".format(base, len(w), len(b), suffix, table), file=out)
            elif variant == "atomic":
                if len(table) <= 6:
                    print("{}/3-4-5/{}.atbw".format(base, table), file=out)
                    if also_dtz:
                        print("{}/3-4-5/{}.atbz".format(base, table), file=out)
                else:
                    print("{}/6-wdl/{}.atbw".format(base, table), file=out)
                    if also_dtz:
                        print("{}/6-dtz/{}.atbz".format(base, table), file=out)
            elif variant == "antichess":
                suffix = "giveaway" if "P" in table else "pawnless"
                ext = "gtb" if "P" in table else "stb"

                if len(table) <= 6:
                    print("{}/2-3-4-5-{}/{}.{}w".format(base, suffix, table, ext), file=out)
                    if also_dtz:
                        print("{}/2-3-4-5-{}/{}.{}z".format(base, suffix, table, ext), file=out)
                else:
                    print("{}/6-{}/{}.{}w".format(base, suffix, table, ext), file=out)
                    if also_dtz:
                        print("{}/6-{}/{}.{}z".format(base, suffix, table, ext), file=out)


if __name__ == "__main__":
    update_dependencies("standard", "tests/regular.csv", "tables/regular/TEST-SOURCE.txt")
    update_dependencies("atomic", "tests/atomic.csv", "tables/atomic/TEST-SOURCE.txt")
    update_dependencies("antichess", "tests/giveaway.csv", "tables/giveaway/TEST-SOURCE.txt")
