import re


class State:

    def __init__(self, x, y, direction):
        self.x = x
        self.y = y
        self.direction = direction


turn_map = {'L': {'N': 'W', 'W': 'S', 'S': 'E', 'E': 'N'}, 'R': {'N': 'E', 'E': 'S', 'S': 'W', 'W': 'N'}}

forward_map = {'N': 1, 'E': 1, 'S': -1, 'W': -1}

format_directions = lambda text: re.sub('([a-z])|([^FLR])', '', text)


def get_end_state(state, directions):
    if len(directions) == 0:
        return state
    else:
        new_sate = move(state, directions[0])
        return get_end_state(new_sate, directions[1:])


def move(state, direction):
    if direction == 'F':
        state = move_forward(state)
    else:
        state.direction = turn_map[direction][state.direction]
    return state


def move_forward(state):
    if state.direction == 'N' or state.direction == 'S':
        state.y += forward_map[state.direction]
    else:
        state.x += forward_map[state.direction]
    return state


def get_turns(state, start_x, start_y):
    if state.y == start_y and state.x == start_x:
        return 0
    elif state.y == start_y:
        return get_turns_when_a_start_and_end_coord_equal(state.x, start_x, state.direction, ['W', 'E', 'N', 'S'])
    elif state.x == start_x:
        return get_turns_when_a_start_and_end_coord_equal(state.y, start_y, state.direction, ['S','N','E','W'])
    elif state.x > 0 and state.direction == 'W' or state.x < 0 and state.direction == 'E' or state.y > 0 and \
            state.direction == 'S' or state.x < 0 and state.direction == 'N':
        return 1
    else:
        return 2


def get_turns_when_a_start_and_end_coord_equal(state_coord, start_coord, direction, directions):
    if state_coord > start_coord and direction == directions[0] or state_coord < start_coord and direction == directions[1]:
        return 0
    elif direction == directions[2] or direction == [3]:
        return 1
    else:
        return 2


def get_shortest_route(state, directions):
    start_x, start_y, = state.x, state.y
    directions = format_directions(directions)
    end_state = get_end_state(state, directions)
    distance = abs(end_state.x - start_x) + abs(end_state.y - start_y) + get_turns(end_state, start_x, start_y)
    return distance


if __name__ == '__main__':
    length = get_shortest_route(State(0, 0, 'N'), 'RFLFRR')

    print(length)
