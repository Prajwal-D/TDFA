#ifndef MOVELIST_HPP
#define MOVELIST_HPP

#include "../Core/Types.hpp"
#include "../Core/Move.hpp"
#include <algorithm>
#include <limits>
#include <ranges>

template<typename T>
constexpr void __FILL(T* begin, std::size_t size, T value)
{
    do { *begin++ = value; }
    while (--size);
}

class MoveList
{
public:
    explicit constexpr MoveList() :head(), tail(&head[0]) { __FILL<Move>(head, MAX_MOVES, std::numeric_limits<Move>::max()); }
    constexpr Move* First() {return std::begin(head);}
    constexpr Move* Last() {return std::end(head);}
    constexpr Move* End()   {return &head[MAX_MOVES];}
    constexpr Move** Current()  {return &tail;}
    constexpr std::size_t Size()const {return tail - head;}
    constexpr bool Contains(Move move)const{return std::ranges::find(head, move) != std::end(head);}
private:
    Move head[MAX_MOVES], *tail;
};



#endif // #ifndef MOVELIST_HPP