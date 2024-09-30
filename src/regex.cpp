#include <iostream>
#include <vector>

namespace regex {
    struct Node {
        enum Kind {
            ALTERNATION, // a|b
            CONCATENATION, // ab
            REPETITION, // a+
            OPTIONAL_REPETITION, // a*
            OPTIONAL, // a?
            ATOM // a
        };

        int kind;
        
        union {
            struct {
                uint32_t first, second;
            };

            char atom;
        };
    };

    struct ParseTree {
        enum Flags {
            BEGIN_ANCHORED = 0x1,
            END_ANCHORED = 0x2,
        };

        int flags = 0;
        std::vector<Node> nodes;
    };

    struct Parser {
        const char *pointer;
        ParseTree *parse_tree;
        std::vector<Node> &nodes;

        Parser(const char *text, ParseTree *parse_tree) : pointer(text), parse_tree(parse_tree), nodes(parse_tree->nodes) {
        }

        bool build_parse_tree() {
            if (*pointer == '^') {
                parse_tree->flags |= ParseTree::BEGIN_ANCHORED;
                ++pointer;
            }

            while (*pointer != 0) {
                if (*pointer == '$') {
                    ++pointer;
                    parse_tree->flags |= ParseTree::END_ANCHORED;
                    break;
                }

                if (!parse()) return false;
            }
        
            return true;
        }

        bool parse() {
            return parse_alternation();
        }

        bool parse_alternation() {
            if (!parse_concatenation()) return false;
            uint32_t first = nodes.size() - 1;

            while (*pointer == '|') {
                ++pointer;
                if (!parse_concatenation()) return false;
                uint32_t second = nodes.size() - 1;

                Node &node = nodes.emplace_back();
                node.kind = Node::ALTERNATION;
                node.first = first;
                node.second = second;

                first = nodes.size() - 1;
            }

            return true;
        }

        bool parse_concatenation() {
            if (!parse_quantifier()) return false;
            uint32_t first = nodes.size() - 1;

            while (parse_quantifier()) {
                uint32_t second = nodes.size() - 1;

                Node &node = nodes.emplace_back();
                node.kind = Node::CONCATENATION;
                node.first = first;
                node.second = second;

                first = nodes.size() - 1;
            }

            return true;
        }

        bool parse_quantifier() {
            if (!parse_unit()) return false;

            uint32_t first = nodes.size() - 1;

            switch (*pointer) {
                case '?': {
                    ++pointer;
                    Node &node = nodes.emplace_back();
                    node.kind = Node::OPTIONAL;
                    node.first = first;

                    break;
                }

                case '*': {
                    ++pointer;
                    Node &node = nodes.emplace_back();
                    node.kind = Node::OPTIONAL_REPETITION;
                    node.first = first;

                    break;
                }

                case '+': {
                    ++pointer;
                    Node &node = nodes.emplace_back();
                    node.kind = Node::REPETITION;
                    node.first = first;

                    break;
                }
            }

            return true;
        }

        bool parse_unit() {
            if (isalpha(tolower(*pointer)) || isdigit(*pointer) || *pointer == '_') {
                Node &node = nodes.emplace_back();
                node.kind = Node::ATOM;
                node.atom = *pointer++;

                return true;
            } else if (*pointer == '(') {
                if (!parse_group()) return false;
            } else {
                return false;
            }

            return true;
        }

        bool parse_group() {
            if (*pointer != '(') return false;
            ++pointer;

            if (!parse()) return false;

            if (*pointer != ')') return false;
            ++pointer;

            return true;
        }
    };

    struct NFA {
        struct Transition {
            // symbol < 256 -> character
            // symbol == -1 -> empty transition
            int symbol;
            std::vector<uint32_t> futures{};
        };

        struct State {
            inline static uint32_t g_id = 0;
            uint32_t id = g_id++;
            uint32_t last_exec = 0; // 1-based (0 is a special value)
            int flags = 0;
            std::vector<Transition> transitions{};

            Transition &get(int symbol) {
                Transition *old = find(symbol);

                if (old) return *old;

                Transition &t = transitions.emplace_back();
                t.symbol = symbol;
                return t;
            }

            Transition *find(int symbol) {
                for (Transition &t : transitions) {
                    if (t.symbol == symbol) return &t;
                }

                return nullptr;
            }

            enum Flags {
                ACCEPTING = 0x1
            };
        };

        std::vector<State> states{};
        uint32_t start_state;

        void debug_print_states() {
            const char *names = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
            for (auto &state : states) {
                std::cerr << "State: " << state.id << '\n';

                for (auto &t : state.transitions) {
                    std::cerr << "Transitions for " << (t.symbol == -1 ? '!' : (char) t.symbol) << " -> ";

                    for (uint32_t x : t.futures) {
                        std::cerr << x << ' ';
                    }

                    std::cerr << '\n';
                }

                std::cerr << '\n';
            }
        }
    };

    struct NFACompiler {
        NFA *machine;

        struct NodeData {
            uint32_t start_state, accept_state;
        };

        std::vector<NodeData> node_stack;

        void compile_tree(ParseTree *parse_tree) {
            emit_node(parse_tree->nodes, parse_tree->nodes.size() - 1);

            // Take the outermost node and connect it to the accept state.
            NodeData last = node_stack.back();
            node_stack.pop_back();

            uint32_t start_state;

            // The better way to implement this is to have the NFAEngine
            // check for matches everywhere in the string unless it happens
            // to be anchored.
            if (!(parse_tree->flags & ParseTree::BEGIN_ANCHORED)) {
                start_state = machine->states.size();
                machine->states.emplace_back();

                // The better way to do this is
                // to figure out which characters
                // may start a match and exclude
                // all of those... this way will
                // waste an executor permanently.
                for (int i = 0; i < 256; i++) {
                    auto &t = machine->states[start_state].get(i);
                    t.futures.push_back(start_state);
                }

                auto &t = machine->states[start_state].get(-1);
                t.futures.push_back(last.start_state);
            } else {
                start_state = last.start_state;
            }

            machine->start_state = start_state;

            uint32_t accept_state = machine->states.size();
            auto &accept = machine->states.emplace_back();
            accept.flags = NFA::State::ACCEPTING;

            if (!(parse_tree->flags & ParseTree::END_ANCHORED)) {
                auto &t = machine->states[last.accept_state].get(-1);
                t.futures.push_back(accept_state);
            } else {
                auto &t = machine->states[last.accept_state].get(0);
                t.futures.push_back(accept_state);
            }
        }

        // a(b|c)
        // (a|b)c
        // ab? === a(b?)

        void emit_node(const std::vector<Node> &nodes, uint32_t node) {
            switch (nodes[node].kind) {
                case Node::ALTERNATION: {
                    emit_node(nodes, nodes[node].first);
                    emit_node(nodes, nodes[node].second);

                    NodeData right_data = node_stack.back();
                    node_stack.pop_back();

                    NodeData left_data = node_stack.back();
                    node_stack.pop_back();

                    NodeData &my_data = node_stack.emplace_back();

                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t1 = machine->states[my_data.start_state].get(-1);
                    t1.futures.push_back(left_data.start_state);
                    t1.futures.push_back(right_data.start_state);

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t2 = machine->states[left_data.accept_state].get(-1);
                    t2.futures.push_back(my_data.accept_state);
                    
                    auto &t3 = machine->states[right_data.accept_state].get(-1);
                    t3.futures.push_back(my_data.accept_state);

                    break;
                }

                case Node::CONCATENATION: {
                    emit_node(nodes, nodes[node].first);
                    emit_node(nodes, nodes[node].second);

                    NodeData right_data = node_stack.back();
                    node_stack.pop_back();

                    NodeData left_data = node_stack.back();
                    node_stack.pop_back();

                    NodeData &my_data = node_stack.emplace_back();

                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t1 = machine->states[my_data.start_state].get(-1);
                    t1.futures.push_back(left_data.start_state);

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t2 = machine->states[left_data.accept_state].get(-1);
                    t2.futures.push_back(right_data.start_state);

                    auto &t3 = machine->states[right_data.accept_state].get(-1);
                    t3.futures.push_back(my_data.accept_state);

                    break;
                }

                case Node::REPETITION: {
                    emit_node(nodes, nodes[node].first);

                    NodeData data = node_stack.back();
                    node_stack.pop_back();

                    NodeData &my_data = node_stack.emplace_back();
                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t1 = machine->states[my_data.start_state].get(-1);
                    t1.futures.push_back(data.start_state);

                    auto &t2 = machine->states[data.accept_state].get(-1);
                    t2.futures.push_back(data.start_state);
                    t2.futures.push_back(my_data.accept_state);

                    break;
                }

                case Node::OPTIONAL_REPETITION: {
                    emit_node(nodes, nodes[node].first);

                    NodeData data = node_stack.back();
                    node_stack.pop_back();

                    NodeData &my_data = node_stack.emplace_back();
                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t1 = machine->states[my_data.start_state].get(-1);
                    t1.futures.push_back(data.start_state);
                    t1.futures.push_back(my_data.accept_state);

                    auto &t2 = machine->states[data.accept_state].get(-1);
                    t2.futures.push_back(my_data.start_state);

                    break;
                }

                case Node::OPTIONAL: {
                    emit_node(nodes, nodes[node].first);

                    NodeData data = node_stack.back();
                    node_stack.pop_back();

                    NodeData &my_data = node_stack.emplace_back();
                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t1 = machine->states[my_data.start_state].get(-1);
                    t1.futures.push_back(data.start_state);
                    t1.futures.push_back(my_data.accept_state);

                    auto &t2 = machine->states[data.accept_state].get(-1);
                    t2.futures.push_back(my_data.accept_state);

                    break;
                }

                case Node::ATOM: {
                    NodeData &my_data = node_stack.emplace_back();

                    my_data.start_state = machine->states.size();
                    machine->states.emplace_back();

                    my_data.accept_state = machine->states.size();
                    machine->states.emplace_back();

                    auto &t = machine->states[my_data.start_state].get(nodes[node].atom);
                    t.futures.push_back(my_data.accept_state);

                    break;
                }
            }
        }
    };
    
    struct NFAEngine {
        NFA *machine;
        uint32_t counter;
        std::vector<uint32_t> a, b;
        std::vector<uint32_t> *cur_states, *next_states;
        const char *ptr;

        bool execute(const char *text) {
            counter = 0;

            a.reserve(machine->states.size());
            a.resize(0);

            b.reserve(machine->states.size());
            b.resize(0);

            for (auto &state : machine->states) {
                state.last_exec = 0;
            }

            cur_states = &a;
            next_states = &b;
            ptr = text;

            next_states->push_back(machine->start_state);

            bool accepted = false;

            while (!(accepted = accepting()) && !next_states->empty()) {
                std::swap(cur_states, next_states);
                next_states->resize(0);
                step();
            }

            return accepted;
        }

        bool accepting() {
            for (uint32_t state : *cur_states) {
                if (machine->states[state].flags & NFA::State::ACCEPTING) {
                    return true;
                }
            }

            return false;
        }

        void step() {
            ++counter;

            for (uint32_t i = 0; i < cur_states->size(); i++) {
                uint32_t state_index = (*cur_states)[i];
                auto &state = machine->states[state_index];
                
                // First find and add all epsilon transitions to cur_states
                // so that we can skip over them.
                NFA::Transition *t = state.find(-1);

                if (t) {
                    cur_states->insert(cur_states->end(), t->futures.begin(), t->futures.end());
                }

                t = state.find(*ptr);

                if (t) {
                    next_states->reserve(t->futures.size());

                    for (uint32_t state_index : t->futures) {
                        auto &state = machine->states[state_index];

                        if (state.last_exec != counter) {
                            state.last_exec = counter;
                            next_states->push_back(state_index);
                        }
                    }
                }
            }

            ++ptr;
        }
    };
}

int main() {
    const char *test_string = "a";
    const char *regex = "^a$";

    regex::ParseTree tree;
    regex::Parser parser(regex, &tree);

    std::cout << "Parsing status: " << (parser.build_parse_tree() ? "success" : "FAILURE") << '\n';

    regex::NFA machine;
    regex::NFACompiler compiler;
    compiler.machine = &machine;
    compiler.compile_tree(&tree);

    // machine.debug_print_states();

    regex::NFAEngine engine;
    engine.machine = &machine;

    std::cout << "Execution status: " << (engine.execute(test_string) ? "matches" : "DOESN'T MATCH") << '\n';
}