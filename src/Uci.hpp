#ifndef UCI_HPP
#define UCI_HPP

#include "Move.hpp"
#include "Types.hpp"
#include "MagicConstants.hpp"
#include "Debug.hpp"
#include "BitBoard.hpp"
#include "Testing.hpp"
#include "Search.hpp"
#include "MoveGen.hpp"
#include <unordered_map>
#include <string>
#include <iostream>

using uci_map = std::unordered_map<std::string, U8>;
namespace UCI
{
    constexpr const char* ENGINE_NAME = "TDFA V1";
    constexpr const char* ENGINE_AUTHOR = "Malik Tremain";
    static inline const uci_map INIT_VALUES = 
    {
        {"uci", 1},
        {"isready", 2},
        {"go", 3},
        {"position", 4},
        {"stop", 5},
        {"ucinewgame", 6},
        {"setoption", 7}
    };
    
    static BB::Position pos(STARTPOS);
    static MoveGen generator(pos);
    static Search search(pos);
    
    
    void loop();

    void HandleUci();
    void HandleIsReady();
    void HandleGo();
    void HandlePosition(const std::string& str);
    void HandleStop();
    void HandleNewGame();
    void HandleSetOption(const std::string& str);
}
#endif // #ifndef UCI_HPP