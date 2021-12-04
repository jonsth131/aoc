#!/usr/bin/env python3
import fileutils


def part1(lst):
    state = gen_bingo_game(lst)
    (draw, winner) = get_winner(state)
    return get_unmarked_sum(winner) * draw


def part2(lst):
    state = gen_bingo_game(lst)
    prev_draw = 0
    prev_winner = {}
    while len(state["boards"]) != 0:
        (prev_draw, prev_winner) = get_winner(state)
        state["boards"].remove(prev_winner)
    return get_unmarked_sum(prev_winner) * prev_draw


def get_unmarked_sum(board):
    return sum([value for (value, marked) in board if marked is False])


def get_winner(state):
    for draw in state["draws"]:
        state["boards"][:] = [mark_draw(i, draw) for i in state["boards"]]
        winner = check_winner(state["boards"])
        if winner is not None:
            return draw, winner
    return None


def check_rows(board):
    for i in range(0, 5, 1):
        row = board[i*5:i*5+5]
        res = all(marked is True for (_, marked) in row)
        if res is True:
            return True
    return False


def check_columns(board):
    for i in range(0, 5):
        column = [board[j+i] for j in range(0, 25, 5)]
        res = all(marked is True for (_, marked) in column)
        if res is True:
            return True
    return False


def check_winner(boards):
    for board in boards:
        if check_rows(board) or check_columns(board):
            return board
    return None


def mark_draw(board, draw):
    for i in range(len(board)):
        if board[i][0] == draw:
            board[i] = (draw, True)
            break
    return board


def gen_bingo_game(lst):
    return {"draws": [int(i) for i in lst[0].split(',')], "boards": gen_boards(lst[2:])}


def gen_boards(lst):
    boards = []
    board = []
    for row in lst:
        if row == '':
            boards.append(board)
            board = []
            continue
        board.extend([(int(i), False) for i in row.split()])
    boards.append(board)
    return boards


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day4.txt")

    print("=== Day 4 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
