#include "thread_pool.hpp"
#include <unistd.h>

class B
{
};

class D
{
};

class A
{
    public:
        D process(bool & stopped, B & data)
        {
            D result;
            std::cout << "here" << std::endl;
            return result;
        }
};

class C
{
    public:
        void output(D & data)
        {
            std::cout << "got data" << std::endl;
        }
};

int main()
{
    C outputObject;
    ThreadPool<A, B, C, D> threadPool(16, 512, outputObject);
    A input1;
    A input2;
    threadPool.input(input1);
    threadPool.input(input2);
    sleep(1);
    threadPool.exit(4000);
    return 0;
}

