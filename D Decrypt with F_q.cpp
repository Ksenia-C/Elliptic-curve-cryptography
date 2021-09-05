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
vector <long long> h;

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


class Polinom {
    vector<ll> val;
    public:
    Polinom() {}
    Polinom(ll num) : val({ num }) {}
    Polinom(vector<ll> mas) : val(mas) {}
    Polinom operator+ (const Polinom& other) const {
        vector<ll> res(max(other.val.size(), this->val.size()) + 1);
        for (int i = 0; i + 1 < res.size(); ++i) {
            res[i] += ((*this)[i] + other[i]) % p;
            res[i] %= p;
        }
        while (res.size() > 1 && res.back() == 0) res.pop_back();
        return Polinom(res);
    }
    Polinom operator* (const Polinom& other) const {
        vector<ll> res(other.val.size() + this->val.size());
        for (int i = 0; i < this->val.size(); ++i) {
            for (int j = 0; j < other.val.size(); ++j) {
                res[i + j] += (this->val[i] * other.val[j]) % p;
                res[i + j] %= p;
            }
        }
        this->get_mod(res);
        while (res.size() != 0 && res.back() == 0) res.pop_back();
        if (res.size() == 0)res = { 0 };
        return Polinom(res);
    }
    bool operator==(const Polinom& other) const {
        return Number(this->val) == Number(other.val);
    }
    bool operator>(const Polinom& other) const {
        return Number(this->val) > Number(other.val);
    }

    ll operator[] (int i) const {
        if (i >= this->val.size()) {
            return 0;
        }
        return this->val[i];
    }

    vector<ll>::const_iterator begin() const {
        return val.begin();
    }
    vector<ll>::const_iterator end() const {
        return val.end();
    }

    size_t size() const {
        return val.size();
    }

    private:
    void get_mod(vector<ll>& mas) const {
        for (int i = mas.size() - 1; i >= 0; --i) {
            if (i + 1 < h.size()) break;
            for (int j = 0; j + 1 < h.size(); ++j) {
                mas[i + j + 1 - h.size()] += mas[i] * h[j] % p;
                mas[i + j + 1 - h.size()] %= p;
            }
            mas[i] = 0;
        }
    }
};

ostream& operator<<(ostream& os, const Polinom& poly)
{
    for (const auto& el : poly) {
        os << el << ' ';
    }
    return os;
}

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

char number_to_char(int num) {
    if (num >= 48 - 48 && num <= 57 - 48)
        return num + 48;
    if (num >= 65 - 55 && num <= 90 - 55)
        return num + 55;
    if (num >= 97 - 61 && num <= 122 - 61)
        return num + 61;
    if (num == 62)
        return 32;
    if (num == 63)
        return 46;
    return '\0';
}

Number from_any_to_10(vector<ll>& num, ll any) {
    Number res(0);
    Number st(1);
    for (auto b : num) {
        res = res + st * b;
        st = st * Number(any);
    }
    return res;
}

vector<ll> from_10_to_any(Number num, ll any) {
    vector<ll> res;
    if (num == 0) return { 0 };
    while (num > 0) {
        res.push_back(num % any);
        num /= any;
    }
    return res;
}

ll mpow(ll a, ll st) {
    if (st == 0) return 1;
    ll res = mpow(a, st / 2);
    res = (res * res);
    if (st % 2) res = (res * a);
    return res;
}

Polinom mpow(Polinom a, ll st) {
    if (st == 0) return 1;
    Polinom res = mpow(a, st / 2);
    res = res * res;
    if (st % 2) res = res * a;
    return res;
}

vector<ll> convert_to_mes(std::string& str) {
    vector<ll> num64;
    num64.reserve(str.size());
    for (char c : str) {
        num64.push_back(char_to_number(c));
    }
    return from_10_to_any(from_any_to_10(num64, 64), p);
}

template<typename T>
T rev(T num) {
    return mpow(num, mpow(p, h.size() - 1) - 2);
}

void Ell_Gamal_coding(vector<Polinom> mes, Polinom g, Polinom k) {
    for (auto num : mes) {
        ll st = rand() % (p - 1) + 1;
        cout << mpow(g, st) << '\n' << num * mpow(k, st) << '\n';
    }
}

vector<Polinom> Ell_Gamal_encoding(ll a) {
    vector<Polinom> mes;
    while (cin.peek() != '\n' && cin.peek() != EOF) {
        vector<ll> r_p, m_p;
        while (cin.peek() != '\n' && cin.peek() != EOF) {
            long long b; cin >> b;
            b = (p + b) % p;
            r_p.push_back(b);
        }
        cin.get();
        Polinom r(r_p);
        while (cin.peek() != '\n' && cin.peek() != EOF) {
            long long b; cin >> b;
            b = (p + b) % p;
            m_p.push_back(b);
        }
        cin.get();
        Polinom m(m_p);
        auto k = mpow(r, a);
        mes.push_back(m * rev(k));
        if (!(k * rev(k) == Polinom(1))) {
            cout << "Yes";
        }
    }
    return mes;
}

void convert_to_str(vector<ll>& mes) {
    auto res = from_10_to_any(from_any_to_10(mes, p), 64);
    for (auto el : res) {
        cout << number_to_char(el);
    }
}

int main() {
    srand(time(NULL));
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    cout.tie(NULL);
    cin >> p;
    cin.get();
    while (cin.peek() != '\n') {
        long long a; cin >> a;
        h.push_back(a);
    }
    while (h.size() > 1 && h.back() == 0) h.pop_back();
    h.back() = (h.back() + p) % p;
    ll a_1 = rev(h.back());
    for (auto& el : h) {
        el = (el * a_1) % p;
    }
    for (int i = 0; i + 1 < h.size(); ++i) {
        h[i] = (p - h[i]) % p;
    }
    h.back() *= -1;
    cin.get();
    ll a; cin >> a;
    cin.get();
    auto polies = Ell_Gamal_encoding(a);
    vector<ll> mes;
    for (auto& poly : polies) {
        for (auto el : poly) {
            mes.push_back(el);
        }
        for (int i = poly.size(); i + 1 < h.size(); ++i) {
            mes.push_back(0);
        }
    }
    convert_to_str(mes);
}