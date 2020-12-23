#include <bits/stdc++.h>

using namespace std;

#define INF numeric_limits<ll>::max()

typedef long long ll;
typedef vector<ll> vl;
typedef vector<char> vc;
typedef pair<ll, ll> pll;


vl load_input() {
    ifstream fin("23.txt");
    char x;
    vl puzzle;
    while (fin >> x) {
        puzzle.push_back(x - '0');    
    }
    return puzzle;
}

ll move(vl &state, ll current) {
    ll first = state[current];
    ll second = state[first];
    ll third = state[second];
    ll suffix = state[third];

    ll dst = current;
    while (dst==current || dst==first || dst==second || dst==third) {
        if (!--dst) dst = state.size() - 1;
    }

    state[current] = suffix;
    state[third] = state[dst];
    state[dst] = first;

    return state[current];
}

ll solve1(vl nums) {
    vl state(10);
    for (ll i = 0; i < nums.size(); i++) {
        state[nums[i]] = nums[(i + 1) % nums.size()];
    }
    
    ll current = nums[0];
    for (ll i = 0; i < 100; i++) {
        current = move(state, current);
    }
    
    ll out = 0;
    ll x = 1;
    while ((x = state[x]) != 1) {
        out = out * 10 + x;
    }
    
    return out;
}

ll solve2(vl nums) {
    vl state(1000001);
    iota(state.begin(), state.end(), 1);
    
    state[1000000] = nums[0];
    for (ll i = 1; i < nums.size(); i++) {
        state[nums[i - 1]] = nums[i];
    }
    state[nums.back()] = nums.size() + 1;
    
    ll current = nums[0];
    for (ll i = 0; i < 10000000; i++) {
        current = move(state, current);
    }
    
    ll first = state[1];
    ll second = state[first];
    
    return first * second;
}

int main() {
    vl puzzle = load_input();
    
    cout << "Part 1: " << solve1(puzzle) << "\n";
    cout << "Part 2: " << solve2(puzzle) << "\n";
    
    return 0;
}