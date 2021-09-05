#include <algorithm>
#include <iostream>
#include <vector>
#include <string>
#include <time.h>

using namespace std;
typedef unsigned long long ll;
ll p;
int sz = 7;
ll max_rank = 10'000'000;

class Number {
    vector<ll> val;
    public:
    Number(ll num) : val({ num }) {}
    Number(vector<ll> mas) : val(mas) {}
    Number operator+ (const Number& other) const {
        vector<ll> res(max(other.val.size(), this->val.size()) + 1);
        for (int i = 0; i + 1 < res.size(); ++i) {
            res[i] += (*this)[i] + other[i];
            if (res[i] >= max_rank) {
                ++res[i + 1];
                res[i] -= max_rank;
            }
        }
        while (res.size() > 1 && res.back() == 0) res.pop_back();
        return Number(res);
    }
    Number operator* (const Number& other) const {
        vector<ll> res(other.val.size() + this->val.size());
        for (int i = 0; i < this->val.size(); ++i) {
            for (int j = 0; j < other.val.size(); ++j) {
                res[i + j] += this->val[i] * other.val[j];
                int ind = i + j;
                while (res[ind] >= max_rank) {
                    if (ind + 1 == res.size()) {
                        res.push_back(0);
                    }
                    res[ind + 1] += res[ind] / max_rank;
                    res[ind] %= max_rank;
                    ++ind;
                }
            }
        }
        while (res.size() != 0 && res.back() == 0) res.pop_back();
        if (res.size() == 0)res = { 0 };
        return Number(res);
    }
    bool operator==(const Number& other) const {
        if (this->val.size() != other.val.size()) {
            return false;
        }
        for (int i = this->val.size() - 1; i >= 0; --i) {
            if (this->val[i] != other.val[i]) {
                return false;
            }
        }
        return true;
    }
    bool operator>(const Number& other) const {
        if (this->val.size() != other.val.size()) {
            return this->val.size() > other.val.size();
        }
        for (int i = this->val.size() - 1; i >= 0; --i) {
            if (this->val[i] != other.val[i]) {
                return this->val[i] > other.val[i];
            }
        }
        return false;
    }

    Number operator/=(const ll d) {
        for (int i = val.size() - 1; i > 0; --i) {
            val[i - 1] += val[i] % d * max_rank;
            val[i] /= d;
        }
        val[0] /= d;
        while (val.size() != 0 && val.back() == 0) val.pop_back();
        if (val.size() == 0)val = { 0 };
        return *this;
    }

    ll operator%(const ll d) const {
        vector<ll> val = this->val;
        for (int i = val.size() - 1; i > 0; --i) {
            val[i - 1] += val[i] % d * max_rank;
            val[i] /= d;
        }
        return val[0] % d;
    }

    ll operator[] (int i) const {
        if (i >= this->val.size()) {
            return 0;
        }
        return this->val[i];
    }
};

int char_to_number(char symbol) {
    if (symbol >= 48 && symbol <= 57)
        return symbol - 48;
    if (symbol >= 65 && symbol <= 90)
        return symbol - 55;
    if (symbol >= 97 && symbol <= 122)
        return symbol - 61;
    if (symbol == 32)
        return 62;
    if (symbol == 46)
        return 63;
    return 64;
}

Number from_64_to_10(vector<int>& num) {
    Number res(0);
    Number st(1);
    for (auto b : num) {
        res = res + st * b;
        st = st * Number(64);
    }
    return res;
}

vector<ll> from_10_to_p(Number num) {
    vector<ll> res;
    if (num == 0) return { 0 };
    while (num > 0) {
        res.push_back(num % p);
        num /= p;
    }
    return res;
}

ll mpow(ll a, int st) {
    if (st == 0) return 1;
    ll res = mpow(a, st / 2);
    res = (res * res) % p;
    if (st % 2) res = (res * a) % p;
    return res;
}

int main() {
    srand(time(NULL));
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    cout.tie(NULL);
    ll g, k; cin >> p >> g >> k;
    cin.get();
    string str;
    getline(cin, str);
    vector<int> num64;
    num64.reserve(str.size());
    for (char c : str) {
        num64.push_back(char_to_number(c));
    }
    auto mes = from_10_to_p(from_64_to_10(num64));
    for (auto num : mes) {
        ll st = rand() % (p - 1) + 1;
        cout << mpow(g, st) << ' ' << num * mpow(k, st) % p << '\n';
    }
}