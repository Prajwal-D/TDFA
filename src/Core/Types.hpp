#ifndef TYPES_HPP
#define TYPES_HPP

#include <cstdint>
#include <array>

using Move      = uint16_t;
using BitBoard  = uint64_t;
using PieceType = uint32_t;

constexpr std::size_t MAX_MOVES = 256;

enum class MD : uint8_t
{
    NORTH,
    NORTH_EAST,
    EAST,
    SOUTH_EAST,
    SOUTH,
    SOUTH_WEST,
    WEST,
    NORTH_WEST,
    NORTHNORTH,
    SOUTHSOUTH
};

struct move_info
{
    constexpr move_info():encoded_move(),count(0){}
    constexpr void add_move(Move m){encoded_move.at(count) = m;++count;}
    // helper function for a debug function. checks if a specified end index is contained within 
    // the encoded moves stored in a move_info instance
    static bool contains_end_index(const move_info& m,uint8_t end_index) 
    {
        for(uint8_t i = 0; i < m.count;++i)
        {
            if(((m.encoded_move[i] & 4032) >> 6) == end_index) return true;
        }
        return false;
    }
    std::array<Move, 7> encoded_move;
    uint16_t count;
};

// This will be specfic class used to decided which direction to test the moves [sq][D::val][index]
enum class D : uint8_t
{
    FILE,
    RANK,
    DIAG,
    ADIAG
};

#endif //#ifndef TYPES_HPP