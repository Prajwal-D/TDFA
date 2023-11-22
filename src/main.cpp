#include <iostream>

#include "Core/MagicConstants.hpp"
#include "MoveGen/MoveList.hpp"
#include "MoveGen/MoveGen.hpp"
#include "Core/Testing.hpp"
#include "Core/Debug.hpp"
#include "Core/Timer.hpp"

constexpr unsigned long long b = 0xFF;
#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define TEST_FEN_LONG "rnbq1rk1/pp2ppbp/6p1/2pp4/2PPnB2/2N1PN2/PP3PPP/R2QKB1R w KQ - 0 8"
#define TEST_FEN_WHITE_SPACE "                            8/8/8/8/4Pp2/8/8/8 w - e3 0 1                              "
#define CALC(x, y) x | (y << 6)

#define TEST1 "rnbqkbnr/pppppppp/8/8/1P6/8/P1PPPPPP/RNBQKBNR b KQkq b3 0 1"
#define TEST2 "rnbqkbnr/1ppppppp/p7/8/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 2"
#define TEST3 "rnbqkbnr/1ppppppp/8/p7/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 1"
int main(void)
{
    /// This is the position generated by the fail on depth 5
    // Debug::PrintBB(103350058913); //us 
    // Debug::PrintBB(10483661951459131520); // them
    
//    PRINTNL(CALC(4,2)); 
//    PRINTNL(CALC(4,6)); 
//    PRINTNL(CALC(60,58)); 
//    PRINTNL(CALC(60,62)); 

    // PerftHandler perft;
    // {
    //     BB::Position pos(START_FEN);

    //     uint64_t time {1};
    //     for(int i =0 ; i < 10; ++i)
    //     {
    //         time = 1;
    //         pos.ImportFen(START_FEN);
    //         {
    //             Timer<std::chrono::microseconds> t(&time);
    //             perft.RunPerft(i, pos);
    //         } 
    //         perft.PrintData();
    //         pos.ResetBoard();
    //         std::cout << std::format("Depth: {}, nodes per second: {:.0f}, Time: {}\n", i, static_cast<double>(perft.GetNodes() * 1'000'000) / static_cast<double>(time), time);    
    //     }
    // }

    const Sq depth = 1;
    // const std::string FEN("rnbqkbnr/p1pppppp/8/Pp6/8/8/1PPPPPPP/RNBQKBNR b KQkq - 0 2");
    const std::string FEN("rnb1kbnr/pppp1ppp/4pq2/8/8/PP6/R1PPPPPP/1NBQKBNR b Kkq - 0 3");
    PerftHandler perft;
    {
        BB::Position pos(FEN);

        uint64_t time {1};
        {
            time = 1;
            pos.ImportFen(FEN);
            {
                Timer<std::chrono::microseconds> t(&time);
                perft.RunPerft(depth, pos);
            } 
            perft.PrintData();
            pos.ResetBoard();
            std::cout << std::format("Depth: {}, nodes per second: {:.0f}, Time: {}\n", depth, static_cast<double>(perft.GetNodes() * 1'000'000) / static_cast<double>(time), time);    
        }
    }


    // for(int i = 0; i < 64;++i)
    // {
    //     for(int j = 0; j < 4;++j)
    //     {
    //         PRINTNL(std::format("Sq: {}, Direction: {}", i, j));
    //         Debug::PrintBB(Magics::SLIDING_ATTACKS_MASK[i][j],i,false);
    //     }
    // }
    // PerftHandler perft;
    // {
    //     BB::Position pos(START_FEN);
    //     {
    //         Timer<std::chrono::microseconds> t;
    //         perft.RunPerft(7,pos);
    //     }
    //     perft.PrintData();
    // }

    // {
    //     BB::Position pos(START_FEN);
    //     Debug::PrintBB(pos.GetPieces<true>());
    //     Debug::PrintBB(pos.GetPieces<false>()); 
    //     PRINTNL(Perft(2,pos));
    // }

    // // {
    //     BB::Position pos("rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1");
    //     MoveGen gen{};
    //     MoveList ml{};
    //     gen.GenerateLegalMoves<false>(pos,ml);
    //     std::cout << ml.len() << std::endl;
    // }

    // Debug::PrintBB(4756084880503341053ull);
    // Debug::PrintBB(18446462598732906496ull);


    // {
    //     BB::Position pos("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1");
    //     MoveGen gen;
    //     MoveList list;
    //     gen.GenerateAllMoves(pos,list);

    //     for(size_t i{0}; i < list.len();++i)
    //     {
    //         std::cout << i << '\t';
    //         Debug::PrintEncodedMoveStr(list[i]);
    //     }
    //     PRINT(list.len());
    // }
        
    // {
    //     BB::Position pos("r3k2r/8/8/8/8/5q2/8/R3K2R w KQkq - 0 1");
    //     MoveGen gen;
    //     MoveList list;
    //     gen.GenerateAllBlackMoves(pos,list);
    //     gen.GenerateAllWhiteMoves(pos,list);

    //     for(size_t i{0}; i < list.len();++i)
    //     {
    //         std::cout << i << '\t';
    //         Debug::PrintEncodedMoveStr(list[i]);
    //     }
    //     PRINT(list.len());
    // }

    // {
    //     BB::Position pos("r3k2r/8/8/6q1/8/8/8/R3K2R w KQkq - 0 1");
    //     MoveGen gen;
    //     MoveList list;
    //     gen.GenerateAllBlackMoves(pos,list);
    //     gen.GenerateAllWhiteMoves(pos,list);

    //     for(size_t i{0}; i < list.len();++i)
    //     {
    //         std::cout << i << '\t';
    //         Debug::PrintEncodedMoveStr(list[i]);
    //     }
    //     PRINT(list.len());
    // }
   
    // {
    //     BB::Position pos("r3k2r/8/8/4Q3/8/8/8/R3K2R w KQkq - 0 1");
    //     MoveGen gen;
    //     MoveList list;
    //     gen.GenerateAllWhiteMoves(pos,list);
    //     gen.GenerateAllBlackMoves(pos,list); 

    //     for(size_t i{0}; i < list.len();++i)
    //     {
    //         std::cout << i << '\t';
    //         Debug::PrintEncodedMoveStr(list[i]);
    //     }
    //     PRINT(list.len());
    // }







    // Debug::PrintBB(pos.GetPieces<true>() | pos.GetPieces<false>());
    // Debug::PrintBB(pos.GetEmptySquares());
    // Debug::PrintBB(gen.GenerateAllWhiteMoves(pos,list));
    // Debug::PrintBB(gen.GenerateAllBlackMoves(pos,list));

    // move_info info;
    // RunTitBoardTest<D::DIAG>(12,"8/8/8/8/8/8/4B3/8 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::ADIAG>(12,"8/8/8/8/8/8/4B3/8 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;
    
    // RunTitBoardTest<D::DIAG>(36,"8/6n1/8/4B3/8/8/8/R7 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::ADIAG>(36,"8/6n1/8/4B3/8/8/8/R7 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::FILE>(6,"8/8/8/8/8/8/8/6R1 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::RANK>(4,"8/8/8/8/8/8/8/P3R1p1 w -- 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::RANK>(2,"8/8/8/8/8/8/8/P1R2P1n w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::FILE>(29,"8/8/8/8/5R2/8/8/8 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::FILE>(29,"8/8/8/5p2/5R2/8/5P2/8 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;

    // RunTitBoardTest<D::DIAG>(24, "8/8/8/8/B7/8/8/8 w - - 0 1",info);
    // PRINT_TIT_TEST_RESULTS;
    
    // BB::Position pos("8/8/8/5p2/5R2/8/5P2/8 w - - 0 1");
    // Debug::PrintBB(pos.GetPieces<true>() | pos.GetPieces<false>());
    // Debug::PrintBB(Magics::CollapsedRanksIndex(pos.GetPieces<true>() | pos.GetPieces<false>()));
    // Debug::PrintBB(4647998506626711584);
    // Debug::PrintBB(18156244167032864);
    return 0;
}
