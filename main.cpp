#include <iostream>
#include <chrono>
#include <sstream>


int main (int argc, char *argv[]) {

    int n = 1000000;
    if (argc > 1) {
        std::stringstream ss(argv[1]);
        ss >> n;
    }


    long long sum = 0;

    auto start = std::chrono::steady_clock::now();

    for (int i=0; i<n; i++){
        for (int j=0; j<i; j++){
            sum += 1;
        }
    }
 
    auto end = std::chrono::steady_clock::now();

    std::cout << "Computed " << n << "-th triangular number " << sum << " in C++ in: "
              << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
              << "ms" << std::endl;

    return 0;
}
