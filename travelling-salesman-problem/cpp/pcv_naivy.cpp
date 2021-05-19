// #ifndef pcv_naivy
// #define pcv_naivy

#include <limits>
#include <string>
#include <cstring>

#include <algorithm>
#include <iostream>
#include <vector>

#include <bits/stdc++.h>
using namespace std;


struct result
{
    string path;
    double cost;
};

void pcv_naivy(vector<int> C, int D)
{
    // 36, 95, 185, 354
    vector<int> I{0, 1, 2, 3};
    
    vector<string> out_p;
    vector<float> out_c;

    vector<int> pi;
    pi = I;
    pi.erase(pi.begin());

    do
    {
        int cost=D[0][pi[0]];
        string orig = to_string(C[0]);
        string dest = to_string(C[pi[0]]);
        string path = orig + "->" + dest + ",";
        for (int i = 0; i < (pi.size() - 1); i++)
        {
            string orig = to_string(C[pi[i]]);
            string dest = to_string(C[pi[i+1]]);
            path = path + orig + "->" + dest + ",";

            cost = cost + D[pi[i]][pi[i + 1]];
        }
        path = path + to_string(C[pi[pi.back()]]) + "->" + to_string(C[0]);
        cost = cost + D[pi[pi.back()]][0];

        out_p.push_back(path);
        out_c.push_back(cost);

        // cout << cost << "\n";
        // cout << path << "\n\n";

    } while (std::next_permutation(pi.begin(), pi.end()));

    int index = std::min_element(out_c.begin(), out_c.end()) - out_c.begin();

    result output;
    output.path = out_p[index];
    output.cost = out_c[index];

    // cout << "Custo: " << output.cost << " path: " << output.path << "\n\n";

    return output;
}


int main() {
    int inf = std::numeric_limits<int>::infinity();
    vector<int> C{36, 95, 185, 354};
    int D[4][4] = {{inf, 351, 2765, 718}, {351, inf, 2546, 517}, {2765, 2546, inf, 1965}, {718, 517, 1965, inf}};
}