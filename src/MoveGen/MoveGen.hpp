#ifndef MOVEGEN_HPP
#define MOVEGEN_HPP

#include <array>
#include <cassert>

#include "../Core/Types.hpp"
#include "../Core/MagicConstants.hpp"
#include "../Core/BitBoard.hpp"
#include "../Core/Move.hpp"
#include "MoveList.hpp"

using Magics::FileOf;
/**
 * The reason that we'd need an array of [64][4][2187] despite the blocker configurations being the same in any direction
 * would be due to the target indexs recorded being different based on the diretions.
 * As of writing this (26/7/23 03:09) the only things that I think would differe between the directions in terms of how
 * we precompute the moves would be how the moves are recorded(target indexes vairing) 
*/
static consteval std::array<std::array<std::array<move_info,2187>,4>,64> PrecomputeTitboards()
{
    std::array<std::array<std::array<move_info,2187>,4>,64> result{};
    for(uint8_t sq = 0; sq < 64; ++sq)
    {
        for(uint16_t us = 0; us < 256;++us)
        {
            for(uint16_t them = 0; them < 256;++them)
            {
                //if our piece is on a piece occupied by an opponents piece, it's an illegal position so we can skip this iteration
                if(us & them) continue;
                
                const uint8_t fileofsq = Magics::FileOf(sq);
                const uint8_t rankofsq = Magics::RankOf(sq);
                //Checks to see if in our piece configuration, there is a piece on the square that we're trying to calc
                //the rank attacks moves for        
                if((us & Magics::BBFileOf(sq)))
                {
                    const uint16_t combined = us | them;
                    move_info rank_attacks{};
                    for(int8_t i = fileofsq + 1; i < 8;++i )
                    {
                        if((us >> i)&1) break; // if we're trying to attack our pieces
                        
                        if(!((combined >> i)&1))//if we're trying to attack empty sq
                        {
                            rank_attacks.add_move(Moves::EncodeMove(sq,sq+(i - fileofsq),Moves::ROOK,true));
                        }

                        if((them >> i)&1) // if we're trying to attack an enemy piece
                        {
                            rank_attacks.add_move(Moves::EncodeMove(sq,sq+(i - fileofsq),Moves::ROOK,true));

                            break;
                        }
                    }
                    for(int8_t i = fileofsq - 1; i > 0;--i )
                    {
                        if((us >> i)&1) break; // if we're trying to attack our pieces
                        
                        if(!((combined >> i)&1))//if we're trying to attack empty sq
                        {
                            rank_attacks.add_move(Moves::EncodeMove(sq,sq + (fileofsq - i), Moves::ROOK, true));
                        }

                        if((them >> i)&1) // if we're trying to attack an enemy piece
                        {
                            rank_attacks.add_move(Moves::EncodeMove(sq, sq + (fileofsq - i), Moves::ROOK, true));

                            break;
                        }
                    }
                    const uint16_t p1 = Magics::base_2_to_3[fileofsq][us & ~Magics::BBFileOf(sq)];
                    const uint16_t p2 = Magics::base_2_to_3[fileofsq][them];
                    result.at(sq).at((uint8_t)D::RANK).at(p1 + p2) = rank_attacks;
                }
                if((us & Magics::BBRankOf(sq)))
                {
                    const uint16_t combined = us | them;
                    move_info file_attacks{};

                    BitBoard diag_atks{};
                    BitBoard adiag_atks{};
                    
                    move_info diag_attacks{};
                    move_info adiag_attacks{};
                    for(int8_t i = rankofsq + 1; i < 8;++i )
                    {
                        if((us >> i)&1) break; // if we're trying to attack our pieces
                        
                        if(!((combined >> i)&1))//if we're trying to attack empty sq
                        {
                            file_attacks.add_move(Moves::EncodeMove(sq, sq + 8 * (i - rankofsq), Moves::ROOK, true));
                            
                            if(Magics::IndexInBounds(sq + 9 * (i - rankofsq)))
                                diag_atks |= Magics::IndexToBB(sq + 9 * (i - rankofsq));

                            if(Magics::IndexInBounds(sq - 7 * (i - rankofsq)))
                                adiag_atks |= Magics::IndexToBB(sq - 7 * (i - rankofsq));
                        }

                        if((them >> i)&1) // if we're trying to attack an enemy piece
                        {
                            file_attacks.add_move(Moves::EncodeMove(sq, sq + 9 * (i - rankofsq), Moves::ROOK, true));
                            
                            if(Magics::IndexInBounds(sq + 9 * (i - rankofsq)))
                                diag_atks |= Magics::IndexToBB(sq + 9 * (i - rankofsq));

                            if(Magics::IndexInBounds(sq - 7 * (i - rankofsq)))
                                adiag_atks |= Magics::IndexToBB(sq - 7 * (i - rankofsq));

                            break;
                        }
                    }
                    for(int8_t i = rankofsq - 1; i > 0;--i )
                    {
                        if((us >> i)&1) break; // if we're trying to attack our pieces
                        
                        if(!((combined >> i)&1))//if we're trying to attack empty sq
                        {
                            file_attacks.add_move(Moves::EncodeMove(sq, sq - 8 * (rankofsq - i), Moves::ROOK, true));
                            
                            if(Magics::IndexInBounds(sq - 9 * (rankofsq - i)))
                                diag_atks |= Magics::IndexToBB(sq - 9 * (rankofsq - i));
                            
                            if(Magics::IndexInBounds(sq + 7 * (rankofsq - i)))
                                adiag_atks |= Magics::IndexToBB(sq + 7 * (rankofsq - i));
                        }

                        if((them >> i)&1) // if we're trying to attack an enemy piece
                        {
                            file_attacks.add_move(Moves::EncodeMove(sq, sq - 8 * (rankofsq - i), Moves::ROOK, true));

                            if(Magics::IndexInBounds(sq - 9 * (rankofsq - i)))
                                diag_atks |= Magics::IndexToBB(sq - 9 * (rankofsq - i));
                            
                            if(Magics::IndexInBounds(sq + 7 * (rankofsq - i)))
                                adiag_atks |= Magics::IndexToBB(sq + 7 * (rankofsq - i));

                            break;
                        }
                    }

                    diag_atks &= Magics::SLIDING_ATTACKS_MASK[sq][(uint8_t)D::DIAG];
                    adiag_atks &= Magics::SLIDING_ATTACKS_MASK[sq][(uint8_t)D::ADIAG];

                    while (diag_atks)
                    {
                       diag_attacks.add_move(Moves::EncodeMove(sq, Magics::FindLS1B(diag_atks),Moves::BISHOP,true));
                       diag_atks = Magics::PopLS1B(diag_atks);
                    }
                    while (adiag_atks)
                    {
                       adiag_attacks.add_move(Moves::EncodeMove(sq, Magics::FindLS1B(adiag_atks),Moves::BISHOP,true));
                       adiag_atks = Magics::PopLS1B(adiag_atks);
                    }

                    const uint16_t p1 = Magics::base_2_to_3[rankofsq][us & ~Magics::BBRankOf(sq)];
                    const uint16_t p2 = 2 * Magics::base_2_to_3[rankofsq][them];
                    result.at(sq).at((uint8_t)D::FILE).at(p1 + p2) = file_attacks;
                    result.at(sq).at((uint8_t)D::DIAG).at(p1 + p2) = diag_attacks;
                    result.at(sq).at((uint8_t)D::ADIAG).at(p1 + p2) = adiag_attacks;
                }
            }
        }
    }
    return result;
}

class MoveGen
{
public:
    MoveGen();
    
    [[nodiscard]] MoveList GenerateAllMoves(const BB::Position& pos);
    
    template<D direction>
    static constexpr move_info& GetMovesForSliding(uint8_t piece_sq, BitBoard us, BitBoard them) noexcept
    {
        return const_cast<move_info&>(SLIDING_ATTACK_CONFIG[piece_sq][static_cast<int>(direction)]
                [
                    //us
                    (Magics::base_2_to_3
                    [(direction == D::RANK) ? Magics::FileOf(piece_sq) 
                    : Magics::RankOf(piece_sq)]
                    [(direction == D::RANK) ? Magics::CollapsedFilesIndex(us & Magics::SLIDING_ATTACKS_MASK[piece_sq][static_cast<int>(direction)])
                    : Magics::CollapsedRanksIndex(us & Magics::SLIDING_ATTACKS_MASK[piece_sq][static_cast<int>(direction)])])
                    +
                    //them
                    (2 * Magics::base_2_to_3
                    [(direction == D::RANK) ? Magics::FileOf(piece_sq) 
                    : Magics::RankOf(piece_sq)]
                    [(direction == D::RANK) ? Magics::CollapsedFilesIndex(them & Magics::SLIDING_ATTACKS_MASK[piece_sq][static_cast<int>(direction)])
                    : Magics::CollapsedRanksIndex(them & Magics::SLIDING_ATTACKS_MASK[piece_sq][static_cast<int>(direction)])])
                ]);
    }
private:

    void UpdateVariables(BitBoard white_pieces, BitBoard black_pieces);

    void WhitePawnMoves(Move** move_list, BitBoard pawns, BitBoard en_passant_target_sq) noexcept;

    void BlackPawnMoves(Move** move_list, BitBoard pawns, BitBoard en_passant_target_sq) noexcept;

    template<bool is_white>
    void BishopMoves(Move** move_list,BitBoard bishops)
    {
        if(!bishops) return;

        while(bishops)
        {
            const uint8_t bishop_index = Magics::FindLS1B(bishops);

            move_info& move = GetMovesForSliding<D::DIAG>(bishop_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::BISHOP>(move.encoded_move[i]);
            }
            
            move = GetMovesForSliding<D::ADIAG>(bishop_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::BISHOP>(move.encoded_move[i]);
            }
            
            bishops = Magics::PopLS1B(bishops);
        }
    }

    template<bool is_white>
    void RookMoves(Move** move_list, BitBoard rooks)
    {
        if(!rooks) return;

        while(rooks)
        {
            const uint8_t rook_index = Magics::FindLS1B(rooks);

            move_info& move = GetMovesForSliding<D::FILE>(rook_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetColour<is_white>(move.encoded_move[i]);
            }

            move = GetMovesForSliding<D::RANK>(rook_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetColour<is_white>(move.encoded_move[i]);
            }
            rooks = Magics::PopLS1B(rooks);
        }
    }

    template<bool is_white>
    void KnightMoves(Move** move_list, BitBoard knights) noexcept
    {
        if(!knights) return;
        while(knights)
        {
            const uint8_t knight_index = Magics::FindLS1B(knights);
            BitBoard possible_move = Magics::KNIGHT_ATTACK_MASKS[knight_index] & (empty_squares_ 
                                        | (is_white ?   (black_pieces_ & ~white_pieces_): 
                                                        (~black_pieces_ & white_pieces_)));
            while(possible_move)
            {
                const uint8_t attack_index = Magics::FindLS1B(possible_move);
                *(*move_list)++ = (is_white ?   Moves::EncodeMove(knight_index,attack_index,Moves::KNIGHT,1):
                                                Moves::EncodeMove(knight_index,attack_index,Moves::KNIGHT,0));
                possible_move = Magics::PopLS1B(possible_move);
            }
            knights = Magics::PopLS1B(knights);
        }
    }

    template<bool is_white>
    void QueenMoves(Move** move_list,BitBoard queens)
    {
        if(!queens) return;

        while(queens)
        {
            const uint8_t queen_index = Magics::FindLS1B(queens);
            
            move_info& move = GetMovesForSliding<D::FILE>(queen_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::QUEEN>(move.encoded_move[i]);
            }

            move = GetMovesForSliding<D::RANK>(queen_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);;
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::QUEEN>(move.encoded_move[i]);
            }


            move = GetMovesForSliding<D::DIAG>(queen_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::QUEEN>(move.encoded_move[i]);
            }

            move = GetMovesForSliding<D::ADIAG>(queen_index,(is_white) ? white_pieces_ : black_pieces_,(is_white) ? black_pieces_ : white_pieces_);
            for(uint8_t i{0}; i < move.count;++i)
            {
                *(*move_list)++ = Moves::SetPieceTypeAndColour<is_white, Moves::QUEEN>(move.encoded_move[i]);
            }
            queens = Magics::PopLS1B(queens);
        }
    }

    void WhiteKingMoves(Move** move_list, BitBoard king) noexcept;

    void BlackKingMoves(Move** move_list, BitBoard king) noexcept;
    
public:    
    constexpr static std::array<std::array<std::array<move_info,2187>,4>,64> SLIDING_ATTACK_CONFIG = PrecomputeTitboards();
    inline static BitBoard EnPassantTargetSquare;
private:
    BitBoard white_pieces_;
    BitBoard black_pieces_;
    BitBoard empty_squares_;
};

#endif // #ifndef MOVEGEN_HPP