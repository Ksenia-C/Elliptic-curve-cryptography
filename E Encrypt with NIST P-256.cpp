#include <algorithm>
#include <iostream>
#include <vector>
#include <string>
#include <time.h>
#include <iomanip>

using namespace std;
typedef unsigned long long ll;

constexpr int digits(int base) noexcept {
    return base <= 1 ? 0 : 1 + digits(base / 10);
}

constexpr int base = 1000'000'000;
constexpr int base_digits = digits(base);

struct bigint {
    // value == 0 is represented by empty z
    vector<int> z; // digits

// sign == 1 <==> value >= 0
// sign == -1 <==> value < 0
    int sign;

    bigint(long long v = 0) { *this = v; }

    bigint& operator=(long long v) {
        sign = v < 0 ? -1 : 1;
        v *= sign;
        z.clear();
        for (; v > 0; v = v / base)
            z.push_back((int)(v % base));
        return *this;
    }

    bigint(const string& s) { read(s); }

    bigint& operator+=(const bigint& other) {
        if (sign == other.sign) {
            for (int i = 0, carry = 0; i < other.z.size() || carry; ++i) {
                if (i == z.size())
                    z.push_back(0);
                z[i] += carry + (i < other.z.size() ? other.z[i] : 0);
                carry = z[i] >= base;
                if (carry)
                    z[i] -= base;
            }
        } else if (other != 0 /* prevent infinite loop */) {
            *this -= -other;
        }
        return *this;
    }

    friend bigint operator+(bigint a, const bigint& b) {
        a += b;
        return a;
    }

    bigint& operator-=(const bigint& other) {
        if (sign == other.sign) {
            if ((sign == 1 && *this >= other) || (sign == -1 && *this <= other)) {
                for (int i = 0, carry = 0; i < other.z.size() || carry; ++i) {
                    z[i] -= carry + (i < other.z.size() ? other.z[i] : 0);
                    carry = z[i] < 0;
                    if (carry)
                        z[i] += base;
                }
                trim();
            } else {
                *this = other - *this;
                this->sign = -this->sign;
            }
        } else {
            *this += -other;
        }
        return *this;
    }

    friend bigint operator-(bigint a, const bigint& b) {
        a -= b;
        return a;
    }

    bigint& operator*=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < z.size() || carry; ++i) {
            if (i == z.size())
                z.push_back(0);
            long long cur = (long long)z[i] * v + carry;
            carry = (int)(cur / base);
            z[i] = (int)(cur % base);
        }
        trim();
        return *this;
    }

    bigint operator*(int v) const { return bigint(*this) *= v; }

    friend pair<bigint, bigint> divmod(const bigint& a1, const bigint& b1) {
        int norm = base / (b1.z.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.z.resize(a.z.size());

        for (int i = (int)a.z.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.z[i];
            int s1 = b.z.size() < r.z.size() ? r.z[b.z.size()] : 0;
            int s2 = b.z.size() - 1 < r.z.size() ? r.z[b.z.size() - 1] : 0;
            int d = (int)(((long long)s1 * base + s2) / b.z.back());
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.z[i] = d;
        }

        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return { q, r / norm };
    }

    bigint operator/(const bigint& v) const { return divmod(*this, v).first; }

    bigint operator%(const bigint& v) const { return divmod(*this, v).second; }

    bigint& operator/=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int)z.size() - 1, rem = 0; i >= 0; --i) {
            long long cur = z[i] + rem * (long long)base;
            z[i] = (int)(cur / v);
            rem = (int)(cur % v);
        }
        trim();
        return *this;
    }

    bigint operator/(int v) const { return bigint(*this) /= v; }

    int operator%(int v) const {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = (int)z.size() - 1; i >= 0; --i)
            m = (int)((z[i] + m * (long long)base) % v);
        return m * sign;
    }

    bigint& operator*=(const bigint& v) {
        *this = *this * v;
        return *this;
    }

    bigint& operator/=(const bigint& v) {
        *this = *this / v;
        return *this;
    }

    bigint& operator%=(const bigint& v) {
        *this = *this % v;
        return *this;
    }

    bool operator<(const bigint& v) const {
        if (sign != v.sign)
            return sign < v.sign;
        if (z.size() != v.z.size())
            return z.size() * sign < v.z.size()* v.sign;
        for (int i = (int)z.size() - 1; i >= 0; i--)
            if (z[i] != v.z[i])
                return z[i] * sign < v.z[i] * sign;
        return false;
    }

    bool operator>(const bigint& v) const { return v < *this; }

    bool operator<=(const bigint& v) const { return !(v < *this); }

    bool operator>=(const bigint& v) const { return !(*this < v); }

    bool operator==(const bigint& v) const { return sign == v.sign && z == v.z; }

    bool operator!=(const bigint& v) const { return !(*this == v); }

    void trim() {
        while (!z.empty() && z.back() == 0)
            z.pop_back();
        if (z.empty())
            sign = 1;
    }

    bool isZero() const { return z.empty(); }

    friend bigint operator-(bigint v) {
        if (!v.z.empty())
            v.sign = -v.sign;
        return v;
    }

    bigint abs() const { return sign == 1 ? *this : -*this; }

    long long longValue() const {
        long long res = 0;
        for (int i = (int)z.size() - 1; i >= 0; i--)
            res = res * base + z[i];
        return res * sign;
    }

    friend bigint gcd(const bigint& a, const bigint& b) { return b.isZero() ? a : gcd(b, a % b); }

    friend bigint lcm(const bigint& a, const bigint& b) { return a / gcd(a, b) * b; }

    void read(const string& s) {
        sign = 1;
        z.clear();
        int pos = 0;
        while (pos < s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = (int)s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            z.push_back(x);
        }
        trim();
    }

    friend istream& operator>>(istream& stream, bigint& v) {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }

    friend ostream& operator<<(ostream& stream, const bigint& v) {
        if (v.sign == -1)
            stream << '-';
        stream << (v.z.empty() ? 0 : v.z.back());
        for (int i = (int)v.z.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.z[i];
        return stream;
    }

    static vector<int> convert_base(const vector<int>& a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int v : a) {
            cur += v * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int)cur);
        while (!res.empty() && res.back() == 0)
            res.pop_back();
        return res;
    }

    bigint operator*(const bigint& v) const {
        return mul_simple(v);
    }

    bigint mul_simple(const bigint& v) const {
        bigint res;
        res.sign = sign * v.sign;
        res.z.resize(z.size() + v.z.size());
        for (int i = 0; i < z.size(); ++i)
            if (z[i])
                for (int j = 0, carry = 0; j < v.z.size() || carry; ++j) {
                    long long cur = res.z[i + j] + (long long)z[i] * (j < v.z.size() ? v.z[j] : 0) + carry;
                    carry = (int)(cur / base);
                    res.z[i + j] = (int)(cur % base);
                }
        res.trim();
        return res;
    }
};

bigint p, a, b;


template<typename Number_T>
pair<Number_T, Number_T> Evclide(const Number_T a, const Number_T b) {
    if (a == 0) {
        return { 0, 1 };
    }
    auto xy = Evclide(b % a, a);
    auto& x1 = xy.first, y1 = xy.second;
    return { y1 - (b / a) * x1, x1 };
}

class Point {
    public:
    bigint x, y, z;
    Point() : x(1), y(0), z(0) {};
    Point(bigint x, bigint y) : x(x), y(y), z(1) {}
    Point(bigint x, bigint y, bigint z) : x(x), y(y), z(z) {}

    Point operator-() const {
        if (z == 0) return Point();
        if (y == 0) return (*this);
        return Point(x, -y, z);
    }

    Point operator+(const Point& other) const {
        if (other.z == 0) return *this;
        if (this->z == 0) return other;
        const auto& x1 = this->x;
        const auto& y1 = this->y;
        const auto& z1 = this->z;
        const auto& x2 = other.x;
        const auto& y2 = other.y;
        const auto& z2 = other.z;
        bigint k;
        bigint deter;
        if ((x1 * z2 - x2 * z1) % p == bigint(0)) {
            if ((y1 * z2 + y2 * z1) % p == bigint(0)) {
                return Point();
            }
            k = bigint(3) * x1 * x1 + a * z1 * z1;
            deter = bigint(2) * y1 * z1;
        } else {
            k = y2 * z1 - y1 * z2;
            deter = x2 * z1 - x1 * z2;
        }
        auto z1z2 = (z1 * z2) % p;
        auto deter2 = (deter * deter) % p;
        bigint x3 = k * k * z1z2 - (x1 * z2 + x2 * z1) * deter2;
        bigint z3 = z1z2 * deter2;
        bigint y3 = k * (x3 * z1 - x1 * z3) + y1 * deter * z3;
        x3 = x3 * z1 * deter;
        z3 = z3 * z1 * deter;
        y3 = (p + y3 % p) % p;
        x3 = (p + x3 % p) % p;
        z3 = (p + z3 % p) % p;
        return -Point(x3, y3, z3);
    }
    bool is_infty_() const {
        return z == 0;
    }
};

bigint rev(const bigint& num) {
    auto res = Evclide(num, p).first;
    return (p + res % p) % p;
}

ostream& operator<<(ostream& os, const Point& num)
{
    if (num.is_infty_()) os << "Z";
    else {
        auto zrev = rev(num.z);
        os << (num.x * zrev) % p << ' ' << (num.y * zrev) % p;
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
    if (symbol == '_')
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
        return '_';
    if (num == 63)
        return 46;
    return '\0';
}

bigint from_any_to_10(vector<ll>& num, const bigint& any) {
    bigint res(0);
    bigint st(1);
    for (auto b : num) {
        res = res + st * b;
        st = st * any;
    }
    return res;
}

vector<bigint> from_10_to_any(bigint num, const bigint& any) {
    vector<bigint> res;
    if (num == 0) return { 0 };
    while (num > 0) {
        res.push_back(num % any);
        num = num / any;
    }
    return res;
}


Point mpow(Point& a, bigint st) {
    if (st == 0) return Point();
    Point res = mpow(a, st / 2);
    res = res + res;
    if (st.z[0] % 2) res = res + a;
    return res;
}

bigint get_rand(int len) {
    string val(len, '0');
    for (auto& el : val) {
        el = '0' + rand() % 10;
    }
    return bigint(val);
}


vector<bigint> convert_to_mes(std::string& str) {
    vector<ll> num64;
    num64.reserve(str.size());
    for (char c : str) {
        num64.push_back(char_to_number(c));
    }
    return from_10_to_any(from_any_to_10(num64, 64), p);
}

bigint deg;


void Ell_Gamal_coding(vector<Point>& mes, Point& g, Point& k) {
    for (auto& num : mes) {
        bigint st = get_rand(30) % (deg - bigint(1)) + bigint(1);
        cout << mpow(g, st) << '\n' << num + mpow(k, st) << '\n';
    }
}

void convert_to_str(vector<ll>& mes) {
    auto res = from_10_to_any(from_any_to_10(mes, p), 64);
    for (auto el : res) {
        cout << number_to_char(el % 64);
    }
}

template<typename Number_T, typename T>
Number_T mpow(const Number_T& a, T st) {
    if (st == 0) return 1;
    Number_T res = mpow(a, st / 2);
    res = res * res;
    if (st % 2) res = res * a;
    return res;
}
template<typename T>
bigint mpow_p(const bigint& a, T st) {
    if (st == 0) return 1;
    bigint res = mpow_p(a, st / 2);
    res *= res;
    if (st % 2) res *= a;
    return res % p;
}

int main() {
    srand(time(NULL));
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    cout.tie(NULL);
    p = mpow(bigint(2), 256) - mpow(bigint(2), 224) + mpow(bigint(2), 192) + mpow(bigint(2), 96) - bigint(1);
    a = bigint(-3);
    b = bigint("41058363725152142129326129780047268409114441015993725554835256314039467401291");
    Point g(bigint("48439561293906451759052585252797914202762949526041747995844080717082404635286"),
        bigint("36134250956749795798585127919587881956611106672985015071877198253568414405109"));
    deg = bigint("115792089210356248762697446949407573529996955224135760342422259061068512044369");
    bigint x, y; cin >> x >> y;
    Point k(x, y);
    int n; cin >> n;
    vector<Point> mes(n);
    bigint p_ = (p + 1) / 4;
    for (int i = 0; i < n; ++i) {
        string str; cin >> str;
        bigint x_p = convert_to_mes(str)[0];
        bigint y_2 = (x_p * x_p * x_p + a * x_p + b) % p;
        bigint y_p = mpow_p(y_2, p_);
        mes[i] = Point(x_p, y_p);
    }
    Ell_Gamal_coding(mes, g, k);
}