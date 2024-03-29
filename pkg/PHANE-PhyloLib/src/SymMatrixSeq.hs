{-# LANGUAGE ImportQualifiedPost #-}

{- |
Description :  Functions to manipulate square symmetric lower diagonal matrices with diagnonal values
                as if they were normal matrices, based on Sequence type
Copyright   :  (c) 2020 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
-}
module SymMatrixSeq (
    empty,
    dim,
    fromLists,
    Matrix,
    SymMatrixSeq.null,
    cols,
    rows,
    (!),
    toLists,
    toRows,
    fromRows,
    toFullLists,
    getFullRow,
    isSymmetric,
    updateMatrix,
    unsafeUpdateMatrix,
    addMatrixRow,
    addMatrices,
    deleteRowsAndColumns,
    showMatrixNicely,
    SymMatrixSeq.map,
    SymMatrixSeq.flatten,
    getFullRowVect,
    SymMatrixSeq.zipWith,
    SymMatrixSeq.zip,
    combine,
    safeIndex,
    makeDefaultMatrix,
    toVectorVector,
) where

import Data.List qualified as L
import Data.Sequence (ViewL (..), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Sort qualified as S
import Data.Vector qualified as V
import LocalSequence qualified as LS


-- | Matrix type as Vector of Vectors
type Matrix a = LS.Seq (LS.Seq a)


-- | empty matrix value from Vector
empty ∷ Matrix a
empty = LS.empty


-- | dim returns dimmension (rows = cols)
dim ∷ (Eq a) ⇒ Matrix a → (Int, Int)
dim inMatrix =
    if SymMatrixSeq.null inMatrix
        then (0, 0)
        else (LS.length inMatrix, LS.length inMatrix)


-- | rows returns number rows in Matrix (rows = cols)
rows ∷ (Eq a) ⇒ Matrix a → Int
rows inM = fst $ dim inM


-- | cols returns number cols in Matrix (rows = cols)
cols ∷ (Eq a) ⇒ Matrix a → Int
cols inM = fst $ dim inM


-- | null returns True of row number is 0
null ∷ (Eq a) ⇒ Matrix a → Bool
null inMatrix = inMatrix == empty


-- | isSymmetric is true by defineition--when created error if not
isSymmetric ∷ (Eq a) ⇒ Matrix a → Bool
isSymmetric inM =
    not (SymMatrixSeq.null inM) || error "Null matrix in isSymmetric"


-- | fromRows creates a lower diagonal matrix (with diagonal)
fromRows ∷ (Eq a, Show a) ⇒ [LS.Seq a] → Matrix a
fromRows inVectList = fromLists $ fmap LS.toList inVectList


{- | toRows converts a Matrix to a list of Vectors
unequal in length
-}
toRows ∷ Matrix a → [LS.Seq a]
toRows = LS.toList


{- | fromLists takes list of list of a and returns lower diagnoal (with diagonal)
matrix as Matrix (Vector of Vectors)
-}
fromLists ∷ (Eq a, Show a) ⇒ [[a]] → Matrix a
fromLists [] = mempty
fromLists inListList@(firstRow : _) =
    let initialSquare = LS.fromList $ LS.fromList <$> inListList
        colsH = length firstRow
        rowsH = length initialSquare
    in  case colsH `compare` rowsH of
            EQ →
                let indexRange = [0 .. rowsH - 1]
                    indexPairs = cartProd indexRange indexRange
                    result
                        | checkSymmetry initialSquare indexPairs = makeLowerDiag initialSquare 0 rowsH
                        | otherwise = error "Input matrix not symmetrical"
                in  result
            _ → error $ unwords ["Input matrix is not square", show (colsH, rowsH), show inListList]


-- | toLists takes a Matrix and returns a list of lists (not all same length)
toLists ∷ Matrix a → [[a]]
toLists = fmap LS.toList . LS.toList


{- | toFullLists takes a Matrix and returns a list of lists of full length
square matrix
-}
toFullLists ∷ (Eq a) ⇒ Matrix a → [[a]]
toFullLists inM
    | SymMatrixSeq.null inM = []
    | otherwise = getFullRow inM <$> [0 .. rows inM - 1]


{- | getFullRow returns a specific full row (is if matrix were square)
as a list
-}
getFullRow ∷ (Eq a) ⇒ Matrix a → Int → [a]
getFullRow inM index =
    if SymMatrixSeq.null inM
        then []
        else
            let firstPart = LS.toList $ inM LS.! index -- initial [0..index] of elements
                restMatrix = LS.drop (index + 1) inM
                restByColumn = LS.toList $ LS.map (LS.! index) restMatrix
            in  firstPart <> restByColumn


{- | getFullRowVect reurns a specific full row (is if matrix were square)
as a Vector
-}
getFullRowVect ∷ (Eq a) ⇒ Matrix a → Int → LS.Seq a
getFullRowVect inM index =
    if SymMatrixSeq.null inM
        then LS.empty
        else
            let firstPart = inM LS.! index -- initial [0..index] of elements
                restMatrix = LS.drop (index + 1) inM
                restByColumn = LS.map (LS.! index) restMatrix
            in  firstPart <> restByColumn


-- | indexing lower diag matrix
(!) ∷ Matrix a → (Int, Int) → a
(!) inM (iIndex, jIndex) =
    if iIndex > jIndex
        then (inM LS.! iIndex) LS.! jIndex
        else (inM LS.! jIndex) LS.! iIndex


-- | indexing lower diag matrix
safeIndex ∷ Matrix a → (Int, Int) → a
safeIndex inM (iIndex, jIndex)
    | iIndex >= length inM = error ("First out of bounds " <> show (iIndex, length inM))
    | jIndex >= length inM = error ("Second out of bounds " <> show (jIndex, length inM))
    | iIndex > jIndex = (inM LS.! iIndex) LS.! jIndex
    | otherwise = (inM LS.! jIndex) LS.! iIndex


{- | makeLowerDiag take a Vector of Vetors (Matrix) and returns a lower diagonal matrix
including diagonal
-}
makeLowerDiag ∷ (Eq a) ⇒ Matrix a → Int → Int → Matrix a
makeLowerDiag inM row numRows
    | SymMatrixSeq.null inM = error "Input matrix is empty in makeLowerDiag"
    | row == numRows = LS.empty
    | otherwise =
        let origRow = inM LS.! row
            newRow = LS.take (row + 1) origRow
        in  newRow <| makeLowerDiag inM (row + 1) numRows


-- | makeDefaultMatrix creates an all 1 wth diagonal 0 matrix of size n x n
makeDefaultMatrix ∷ Int → Matrix Int
makeDefaultMatrix n =
    let row = replicate n 1
        rowList = replicate n row
        initialMattrix = fromLists rowList
        updateIndexList = [0 .. (n - 1)]
        zeroList = replicate n 0
        updateList = zip3 updateIndexList updateIndexList zeroList
    in  updateMatrix initialMattrix updateList


-- | cartesian product of two lists
cartProd ∷ [a] → [a] → [(a, a)]
cartProd xs ys = [(x, y) | x ← xs, y ← ys]


{- | checkSymmetry takes a Vector of VEctors of a
and checks for symmetry
-}
checkSymmetry ∷ (Eq a, Show a) ⇒ Matrix a → [(Int, Int)] → Bool
checkSymmetry inVV
    | SymMatrixSeq.null inVV = const True -- Holds vacuously
    | otherwise = \case
        [] → True
        (iIndex, jIndex) : otherIndices →
            let original = (inVV LS.! iIndex) LS.! jIndex
                reversed = (inVV LS.! jIndex) LS.! iIndex
            in  if original == reversed
                    then checkSymmetry inVV otherIndices
                    else
                        error $
                            unwords
                                [ "Matrix is not symmetrical:"
                                , show (iIndex, jIndex)
                                , "=>"
                                , show original
                                , "/="
                                , show reversed
                                ]


{- | addMatrixRow add a row to existing matrix to extend Matrix dimension
used when adding HTU distances to existing distance matrix as Wagner tree is built
-}
addMatrixRow ∷ (Eq a) ⇒ Matrix a → LS.Seq a → Matrix a
addMatrixRow inM newRow =
    if LS.null newRow
        then inM
        else inM |> newRow


-- | addMatrices  adds a Matrix to existing matrix to extend Matrix dimension
addMatrices ∷ (Eq a) ⇒ Matrix a → Matrix a → Matrix a
addMatrices inM newMatrix =
    if SymMatrixSeq.null newMatrix
        then inM
        else inM <> newMatrix


-- | reIndexTriple taske (i,j,k) and returns (max i j, min i j, k)
reIndexTriple ∷ (Ord a) ⇒ (a, a, b) → (a, a, b)
reIndexTriple trip@(iIndex, jIndex, value) =
    if iIndex > jIndex
        then trip
        else (jIndex, iIndex, value)


{- | updateMatrix takes a list of triples and update matrix
update all at once checking for bounds
could naively do each triple in turn, but would be alot of copying
-}
updateMatrix ∷ (Show a, Ord a) ⇒ Matrix a → [(Int, Int, a)] → Matrix a
updateMatrix inM modList = case S.uniqueSort $ reIndexTriple <$> modList of
    [] → inM
    orderedTriples@((minRow, _, _) : others) → case minRow `compare` 0 of
        LT → error $ "Update matrix out of bounds: " <> show orderedTriples
        _ → case L.unsnoc others of
            Nothing → error $ "Update matrix missing index " <> show orderedTriples
            Just (_, (maxRow, _, _)) → case maxRow `compare` rows inM of
                LT →
                    let firstPart = LS.unsafeTake minRow inM
                        otherPart = LS.unsafeDrop minRow inM
                        modifiedRemainder = updateRows otherPart orderedTriples minRow
                    in  addMatrices firstPart modifiedRemainder
                _ →
                    error $
                        unwords
                            [ "Update matrix out of bounds, row ="
                            , show $ rows inM
                            , "and trying to update row"
                            , show maxRow
                            ]


-- | unsafeUpdateMatrix unsafe version of updateMatrix
unsafeUpdateMatrix ∷ (Show a, Ord a) ⇒ Matrix a → [(Int, Int, a)] → Matrix a
unsafeUpdateMatrix inM inTriples = case S.uniqueSort $ reIndexTriple <$> inTriples of
    [] → inM
    orderedTriples@((minRow, _, _) : _) →
        let firstPart = LS.unsafeTake minRow inM
            otherPart = LS.unsafeDrop minRow inM
            modifiedRemainder = updateRows otherPart orderedTriples minRow
        in  addMatrices firstPart modifiedRemainder


{- | updateRows takes the section of the matrix containing rows that wil be modified
(some not) and modifes or copies rows and rerns a Matrix (vector of roow vectors)
-}
updateRows ∷ (Show a, Eq a) ⇒ Matrix a → [(Int, Int, a)] → Int → Matrix a
updateRows inM tripList currentRow = case tripList of
    [] → inM
    (rowIndex, columnIndex, value) : otherTriples → case Seq.viewl inM of
        EmptyL → error $ "Matrix is empty and there are modifications that remain: " <> show tripList
        -- account for multiple modifications to same row
        firstOrigRow :< otherRows →
            let (selectedRow, selectedList)
                    | currentRow /= rowIndex = (firstOrigRow, tripList)
                    | otherwise = modifyRow firstOrigRow columnIndex value currentRow otherTriples
            in  selectedRow <| updateRows otherRows selectedList (currentRow + 1)


{- | modifyRow takes an initial modification (column and value) and then checks to see if there are more modifications in that
row (rowNumber) in the remainder of the list of modifications, returning the new row and mod list as a pair
assumes that sorted triples sort by first, second, then third elements
-}
modifyRow ∷ LS.Seq a → Int → a → Int → [(Int, Int, a)] → (LS.Seq a, [(Int, Int, a)])
modifyRow inRow colIndex value rowNumber modList =
    if colIndex >= LS.length inRow
        then error ("Column to modify is outside length of row " <> show (rowNumber, colIndex))
        else
            let firstPart = LS.unsafeTake colIndex inRow
                remainderPart = LS.unsafeDrop (colIndex + 1) inRow
                newRow = firstPart <> (value <| remainderPart)
            in  if L.null modList
                    then (newRow, modList)
                    else continueRow (firstPart |> value) inRow (colIndex + 1) rowNumber modList


{- | continueRow continues to modify a row with multiple column modifcations
assumes that sorted triples sorted by first, second, then third elements
-}
continueRow ∷ LS.Seq a → LS.Seq a → Int → Int → [(Int, Int, a)] → (LS.Seq a, [(Int, Int, a)])
continueRow partRow origRow colIndex rowNumber modList
    | colIndex == length origRow = (partRow, modList)
    | otherwise = case modList of
        [] → (partRow <> LS.drop colIndex origRow, modList)
        (nextRowNumber, _, _) : _ | nextRowNumber /= rowNumber → (partRow <> LS.drop colIndex origRow, modList)
        (_, nextColIndex, nextValue) : otherMods →
            let (endValue, nextList)
                    | nextColIndex /= colIndex = (origRow LS.! colIndex, modList)
                    | otherwise = (nextValue, otherMods)
            in  continueRow (partRow |> endValue) origRow (colIndex + 1) rowNumber nextList


-- | makeNiceRow pretty preints a list
makeNiceRow ∷ (Eq a, Show a) ⇒ LS.Seq a → String
makeNiceRow aVect =
    if LS.null aVect
        then "\n"
        else show (LS.head aVect) <> " " <> makeNiceRow (LS.tail aVect)


-- | showNicely pretty prins matrix
showMatrixNicely ∷ (Show a, Eq a) ⇒ Matrix a → String
showMatrixNicely inM =
    let mRows = rows inM
        mCols = cols inM
        niceRows = LS.map makeNiceRow inM
    in  ("Dimensions: :" <> show mRows <> " " <> show mCols <> "\n" <> concat niceRows)


{- | deleteRowsAndColumns take a list of rows (and same index for columns)
to delete from Matrix. Uses lisyt to do in single pass
-}
deleteRowsAndColumns ∷ (Show a, Eq a) ⇒ Matrix a → [Int] → Matrix a
deleteRowsAndColumns inM deleteList =
    if L.null deleteList
        then inM
        else deleteRC inM deleteList (rows inM) 0


-- | deleteRC takes matri delete list and counter to delte coumns and rows
deleteRC ∷ (Show a, Eq a) ⇒ Matrix a → [Int] → Int → Int → Matrix a
deleteRC inM deleteList origRows rowCounter =
    if rowCounter == origRows
        then empty
        else
            let firstRow = LS.head inM
                toKeep = rowCounter `L.notElem` deleteList
                newRow = deleteColumn firstRow deleteList (rowCounter + 1) 0
            in  if toKeep
                    then newRow <| deleteRC (LS.tail inM) deleteList origRows (rowCounter + 1)
                    else deleteRC (LS.tail inM) deleteList origRows (rowCounter + 1)


{- | deleteColumn takes a row of a matrix (lower diagnonal), its length,
a list of cilumns to delete and a column counter and creates a new row
-}
deleteColumn ∷ (Show a, Eq a) ⇒ LS.Seq a → [Int] → Int → Int → LS.Seq a
deleteColumn origRow deleteList rowLength colCounter =
    if colCounter == rowLength
        then LS.empty
        else
            let firstValue = LS.head origRow
                toKeep = colCounter `L.notElem` deleteList
            in  if toKeep
                    then firstValue <| deleteColumn (LS.tail origRow) deleteList rowLength (colCounter + 1)
                    else deleteColumn (LS.tail origRow) deleteList rowLength (colCounter + 1)


-- | map maps a function over the matrix returning a matrix of new type
map ∷ (Eq a) ⇒ (a → b) → Matrix a → Matrix b
map f m =
    if SymMatrixSeq.null m
        then empty
        else LS.map (LS.map f) m


-- | flatten concats rows of matrix to make a single Vector
flatten ∷ (Eq a) ⇒ Matrix a → LS.Seq a
flatten m =
    if SymMatrixSeq.null m
        then LS.empty
        else LS.concat m


-- | zip takes two matrices and zips into a matrix of pairs
zip ∷ (Eq a, Eq b) ⇒ Matrix a → Matrix b → Matrix (a, b)
zip m1 m2
    | dim m1 /= dim m2 = error ("Cannot zip matrices with unequal dimensions " <> show (dim m1) <> " " <> show (dim m2))
    | LS.null m1 = LS.empty
    | otherwise =
        let m1r = LS.head m1
            m2r = LS.head m2
            newRow = LS.zip m1r m2r
        in  newRow <| SymMatrixSeq.zip (LS.tail m1) (LS.tail m2)


-- | zip takes two matrices and zips into a matrix using f
zipWith ∷ (Eq a, Eq b) ⇒ (a → b → c) → Matrix a → Matrix b → Matrix c
zipWith f m1 m2
    | dim m1 /= dim m2 = error ("Cannot zip matrices with unequal dimensions " <> show (dim m1) <> " " <> show (dim m2))
    | LS.null m1 = LS.empty
    | otherwise =
        let m1r = LS.head m1
            m2r = LS.head m2
            newRow = LS.map g $ LS.zip m1r m2r
        in  newRow <| SymMatrixSeq.zipWith f (LS.tail m1) (LS.tail m2)
    where
        g (a, b) = f a b


{- | combine takes an operator f (Enforcing Num as opposed to zipWith) and two matrices
applying f to each element of the two matrices M1 f M2
to create the output
-}
combine ∷ (Eq a) ⇒ (a → a → a) → Matrix a → Matrix a → Matrix a
combine f m1 m2
    | SymMatrixSeq.null m1 = error "Null matrix 1 in combine"
    | SymMatrixSeq.null m2 = error "Null matrix 2 in combine"
    | dim m1 /= dim m2 = error ("Cannot combine matrices with unequal dimensions " <> show (dim m1) <> " " <> show (dim m2))
    | otherwise = SymMatrixSeq.map g $ SymMatrixSeq.zip m1 m2
    where
        g (a, b) = f a b


-- | convert to Vector based matrix
toVectorVector ∷ Matrix a → V.Vector (V.Vector a)
toVectorVector inMat = LS.toVector $ LS.map LS.toVector inMat
