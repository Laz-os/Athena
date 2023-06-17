'Sortrows2d
'GetOneLine
'SwapArrayRows
'SortThisArray http://vbcity.com/forums/t/67392.aspx
'SortArr2D1Key https://en.wikibooks.org/wiki/Visual_Basic_for_Applications/Bubble_Sort_on_One_Key
'unifrnd array from random uniform distribution (matlab)
'expandarray
'Min
'Max
'interp2d 'https://en.wikipedia.org/wiki/Bilinear_interpolation

Public Class Functions

    Public Shared Function GetOneLine(array2D As Object, lineIndex As Integer, choice As String) As Object
        ' returning one column or row of a 2D array
        ' array2D: 2 dimensional Array
        ' lineIndex: the index of column or row, starting at 0
        ' choice: "c" for column or "r" for row

        Dim i, n As Integer
        Dim oneLine As Object


        If choice = "c" Then

            n = UBound(array2D, 2)
            ReDim oneLine(n)

            For i = 0 To n
                oneLine(i) = array2D(lineIndex, i)
            Next

            GetOneLine = oneLine

        End If


        If choice = "r" Then

            n = UBound(array2D, 1)
            ReDim oneLine(n)

            For i = 0 To n
                oneLine(i) = array2D(i, lineIndex)
            Next

            GetOneLine = oneLine

        End If

    End Function

    Public Shared Function SwapArrayRows(Arr As Object, Row1 As Long, Row2 As Long) As Object
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' SwapArrayRows
        ' This function returns an array based on Arr with Row1 and Row2 swapped.
        ' It returns the result array or NULL if an error occurred.
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Dim V As Object
        Dim Result As Object
        Dim RowNdx As Long
        Dim ColNdx As Long
        '''''''''''''''''''''''''''''''''
        ' If Row1 = Row2, just return the
        ' array and exit. Nothing to do.
        '''''''''''''''''''''''''''''''''
        If Row1 = Row2 Then
            SwapArrayRows = Arr
            Exit Function
        End If

        '''''''''''''''''''''''''''''''''''''''''
        ' Redim V to the number of columns.
        '''''''''''''''''''''''''''''''''''''''''
        ReDim V(0 To UBound(Arr, 2))
        '''''''''''''''''''''''''''''''''''''''''
        ' Put Row1 in V
        '''''''''''''''''''''''''''''''''''''''''
        For ColNdx = LBound(Arr, 2) To UBound(Arr, 2)
            V(ColNdx) = Arr(Row1, ColNdx)
            Result(Row1, ColNdx) = Arr(Row2, ColNdx)
            Result(Row2, ColNdx) = V(ColNdx)
        Next ColNdx

        SwapArrayRows = Result

    End Function

    Public Shared Function SortThisArray(inputArray(,) As Double, firstSort As Integer, secondSort As Integer, sortDescending As Boolean) As Double(,)
        'inputArray -Variant - Any two-dimensional array
        'firstSort -Integer - The first column To sort by, values begin with 0
        'secondSort -Integer - The second column To sort by, use - 1 To disable, values begin with 0
        'sortDescending -Boolean - False = sort ascending, True = sort descending
        Dim min As Double
        Dim y As String
        Dim z As Integer
        Dim n As Integer
        Dim i As Integer
        Dim j As Integer
        Dim Dummy As Double(,) = New Double(,) {}
        Dim w(UBound(inputArray, 1), UBound(inputArray, 2)) As Double
        If UBound(inputArray, 2) < firstSort Or UBound(inputArray, 2) < secondSort Then
            SortThisArray = inputArray
            Exit Function
        End If
        If firstSort < 0 Then
            firstSort = 0
        End If
        If secondSort < -1 Then
            secondSort = -1
        End If
        If firstSort = secondSort Then
            secondSort = -1
        End If
        For i = 0 To (UBound(inputArray, 1) - 1)
            min = inputArray(0, firstSort)
            z = 0
            If secondSort <> -1 Then
                y = inputArray(0, secondSort)
            End If
            For n = 1 To UBound(inputArray, 1)
                If sortDescending = True Then
                    If inputArray(firstSort, n) > min Then
                        min = inputArray(n, firstSort)
                        z = n
                        If secondSort <> -1 Then
                            y = inputArray(n, secondSort)
                        End If
                    End If
                    If inputArray(n, firstSort) = min And secondSort <> -1 Then
                        If inputArray(n, secondSort) > y Then
                            min = inputArray(n, firstSort)
                            z = n
                            y = inputArray(n, secondSort)
                        End If
                    End If
                Else
                    If inputArray(n, firstSort) < min Then
                        min = inputArray(n, firstSort)
                        z = n
                        If secondSort <> -1 Then
                            y = inputArray(n, secondSort)
                        End If
                    End If
                    If inputArray(n, firstSort) = min And secondSort <> -1 Then
                        If inputArray(n, secondSort) < y Then
                            min = inputArray(n, firstSort)
                            z = n
                            y = inputArray(n, secondSort)
                        End If
                    End If
                End If
            Next n
            For j = 0 To UBound(inputArray, 2)
                w(i, j) = inputArray(z, j)
                inputArray(z, j) = inputArray(UBound(inputArray, 1), j)
            Next j

            ReDim Dummy(UBound(inputArray, 1) - 1, UBound(inputArray, 2))
            For k = 0 To UBound(inputArray, 1) - 1
                For b = 0 To UBound(inputArray, 2)
                    Dummy(k, b) = inputArray(k, b)
                Next
            Next
            ReDim inputArray(UBound(inputArray, 1) - 1, UBound(inputArray, 2))
            For k = 0 To UBound(inputArray, 1)
                For b = 0 To UBound(inputArray, 2)
                    inputArray(k, b) = Dummy(k, b)
                Next
            Next

        Next i
        For j = 0 To UBound(inputArray, 2)
            w(UBound(w, 1), j) = inputArray(0, j)
        Next j
        SortThisArray = w
    End Function

    Public Shared Function ExpandArray(Arr(,) As Double, WhichDim As Long, AdditionalElements As Long,
        FillValue As Object) As Double(,)
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' ExpandArray
        ' This expands a two-dimensional array in either dimension. It returns the result
        ' array if successful, or NULL if an error occurred. The original array is never
        ' changed.
        ' Paramters:
        ' --------------------
        ' Arr                   is the array to be expanded.
        '
        ' WhichDim              is either 1 for additional rows or 2 for
        '                       additional columns.
        '
        ' AdditionalElements    is the number of additional rows or columns
        '                       to create.
        '
        ' FillValue             is the value to which the new array elements should be
        '                       initialized.
        '
        ' You can nest calls to Expand array to expand both the number of rows and
        ' columns. E.g.,
        '
        ' C = ExpandArray(ExpandArray(Arr:=A, WhichDim:=1, AdditionalElements:=3, FillValue:="R"), _
        '    WhichDim:=2, AdditionalElements:=4, FillValue:="C")
        ' This first adds three rows at the bottom of the array, and then adds four
        ' columns on the right of the array.
        '
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Dim Result As Double(,) = New Double(,) {}
        Dim RowNdx As Long
        Dim ColNdx As Long
        Dim ResultRowNdx As Long
        Dim ResultColNdx As Long
        Dim NumRows As Long
        Dim NumCols As Long
        Dim NewUBound As Long

        Const ROWS_ As Long = 1
        Const COLS_ As Long = 2

        NumRows = UBound(Arr, 1) - LBound(Arr, 1) + 1
        NumCols = UBound(Arr, 2) - LBound(Arr, 2) + 1

        If WhichDim = ROWS_ Then
            '''''''''''''''
            ' Redim Result.
            '''''''''''''''
            ReDim Result(0 To UBound(Arr, 1) + AdditionalElements, 0 To UBound(Arr, 2))
            ''''''''''''''''''''''''''''''
            ' Transfer Arr array to Result
            ''''''''''''''''''''''''''''''
            For RowNdx = LBound(Arr, 1) To UBound(Arr, 1)
                For ColNdx = LBound(Arr, 2) To UBound(Arr, 2)
                    Result(RowNdx, ColNdx) = Arr(RowNdx, ColNdx)
                Next ColNdx
            Next RowNdx
            '''''''''''''''''''''''''''''''
            ' Fill the rest of the result
            ' array with FillValue.
            '''''''''''''''''''''''''''''''
            For RowNdx = UBound(Arr, 1) + 1 To UBound(Result, 1)
                For ColNdx = LBound(Arr, 2) To UBound(Arr, 2)
                    Result(RowNdx, ColNdx) = FillValue
                Next ColNdx
            Next RowNdx
        Else
            '''''''''''''''
            ' Redim Result.
            '''''''''''''''
            ReDim Result(0 To UBound(Arr, 1), UBound(Arr, 2) + AdditionalElements)
            ''''''''''''''''''''''''''''''
            ' Transfer Arr array to Result
            ''''''''''''''''''''''''''''''
            For RowNdx = LBound(Arr, 1) To UBound(Arr, 1)
                For ColNdx = LBound(Arr, 2) To UBound(Arr, 2)
                    Result(RowNdx, ColNdx) = Arr(RowNdx, ColNdx)
                Next ColNdx
            Next RowNdx
            '''''''''''''''''''''''''''''''
            ' Fill the rest of the result
            ' array with FillValue.
            '''''''''''''''''''''''''''''''
            For RowNdx = LBound(Arr, 1) To UBound(Arr, 1)
                For ColNdx = UBound(Arr, 2) + 1 To UBound(Result, 2)
                    Result(RowNdx, ColNdx) = FillValue
                Next ColNdx
            Next RowNdx

        End If
        ''''''''''''''''''''
        ' Return the result.
        ''''''''''''''''''''
        ExpandArray = Result

    End Function

    Public Shared Function Unifrnd(a As Double, b As Double)
        Dim r As Double
        Dim a2 = a / 2
        Dim b2 = b / 2
        Dim mu As Double = a2 + b2
        Dim sig As Double = b2 - a2
        r = mu + sig * (2 * Rnd() - 1)
        Return r
    End Function

    Public Shared Function Min(Ar() As Double)
        Dim min_value As Double = Ar(LBound(Ar))
        For i = LBound(Ar) To UBound(Ar)
            If min_value > Ar(i) Then min_value = Ar(i)
        Next
        Return min_value
    End Function

    Public Shared Function Max(Ar() As Double)
        Dim max_value As Double = Ar(LBound(Ar))
        For i = LBound(Ar) To UBound(Ar)
            If max_value < Ar(i) Then max_value = Ar(i)
        Next
        Return max_value
    End Function

    Public Shared Function interp2d(ArX() As Double, ArY() As Double, ArZ() As Double, X As Double, Y As Double) As Double

        'Linear 2d interpolation (Bilinear interpolation)
        '///////////
        'for other interpolation algorithms:
        'https://github.com/danielguterding/pytricubic/tree/master/src
        'https://github.com/igmhub/likely
        'https://github.com/DurhamDecLab/ARBInterp
        '///////////

        If X < ArX.Min Then
            X = ArX.Min
        ElseIf X > ArX.Max Then
            X = ArX.Max
        End If
        If Y < ArY.Min Then
            Y = ArY.Min
        ElseIf Y > ArY.Max Then
            Y = ArY.Max
        End If

        Dim MasterArray(ArX.GetUpperBound(0), 2) As Double
        For i = 0 To ArX.GetUpperBound(0)
            MasterArray(i, 0) = ArX(i)
            MasterArray(i, 1) = ArY(i)
            MasterArray(i, 2) = ArZ(i)
        Next

        MasterArray = SortThisArray(MasterArray, 0, 1, False)
        Dim ArX_(ArX.GetUpperBound(0)) As Double
        Dim ArY_(ArX.GetUpperBound(0)) As Double
        Dim Arz_(ArX.GetUpperBound(0)) As Double
        For i = 0 To ArX.GetUpperBound(0)
            ArX_(i) = MasterArray(i, 0)
            ArY_(i) = MasterArray(i, 1)
            Arz_(i) = MasterArray(i, 2)
        Next

        Dim X1 As Double = Array.FindLast(Of Double)(ArX_, Function(value) value <= X)
        Dim X2 As Double = Array.Find(Of Double)(ArX_, Function(value) value >= X)
        Dim f11, f12, f21, f22, c As Double

        If X1 = X2 Then
            'only the data from the corresponding Reynolds are to be used
            Dim iX1 As Integer = Array.IndexOf(Of Double)(ArX_, X1)
            Dim iX2 As Integer = Array.LastIndexOf(Of Double)(ArX_, X2)
            Dim ArY1(iX2 - iX1) As Double
            Dim ArZ1(iX2 - iX1) As Double
            Array.Copy(ArY_, iX1, ArY1, 0, ArY1.Length)
            Array.Copy(Arz_, iX1, ArZ1, 0, ArY1.Length)
            c = interp1d(ArY1, ArZ1, Y)
            Return c
        End If

        MasterArray = SortThisArray(MasterArray, 1, 0, False)
        For i = 0 To ArX.GetUpperBound(0)
            ArX_(i) = MasterArray(i, 0)
            ArY_(i) = MasterArray(i, 1)
            Arz_(i) = MasterArray(i, 2)
        Next

        Dim Y1 As Double = Array.FindLast(Of Double)(ArY_, Function(value) value <= Y)
        Dim Y2 As Double = Array.Find(Of Double)(ArY_, Function(value) value >= Y)

        If Y1 = Y2 Then

            Dim iY1 As Integer = Array.IndexOf(Of Double)(ArY_, Y1)
            Dim iY2 As Integer = Array.LastIndexOf(Of Double)(ArY_, Y2)
            Dim ArX1(iY2 - iY1) As Double
            Dim ArZ1(iY2 - iY1) As Double
            Array.Copy(ArX_, iY1, ArX1, 0, ArX1.Length)
            Array.Copy(Arz_, iY1, ArZ1, 0, ArX1.Length)
            c = interp1d(ArX1, ArZ1, X)
            Return c
        End If

        MasterArray = SortThisArray(MasterArray, 0, 1, False)
        For i = 0 To ArX.GetUpperBound(0)
            ArX_(i) = MasterArray(i, 0)
            ArY_(i) = MasterArray(i, 1)
            Arz_(i) = MasterArray(i, 2)
        Next

        For i = 1 To ArX.Length

            If ArX_(i - 1) = X1 Then

                If ArY_(i - 1) = Y1 Then
                    f11 = Arz_(i - 1)
                ElseIf ArY_(i - 1) = Y2 Then
                    f12 = Arz_(i - 1)
                End If

            ElseIf ArX_(i - 1) = X2 Then

                If ArY_(i - 1) = Y1 Then
                    f21 = Arz_(i - 1)
                ElseIf ArY_(i - 1) = Y2 Then
                    f22 = Arz_(i - 1)
                End If
            End If
        Next

        Dim c1 As Double = (X2 - X) * f11 / (X2 - X1) + (X - X1) * f21 / (X2 - X1)

        Dim c2 As Double = (X2 - X) * f12 / (X2 - X1) + (X - X1) * f22 / (X2 - X1)

        c = (Y2 - Y) * c1 / (Y2 - Y1) + (Y - Y1) * c2 / (Y2 - Y1)

        Return c

    End Function

    Public Shared Function interp1d(ArX() As Double, ArY() As Double, X As Double) As Double
        Dim c, f1, f2 As Double

        If X < ArX.Min Then
            X = ArX.Min
        ElseIf X > ArX.Max Then
            X = ArX.Max
        End If
        Dim X1 As Double = Array.FindLast(Of Double)(ArX, Function(value) value <= X)
        Dim X2 As Double = Array.Find(Of Double)(ArX, Function(value) value >= X)
        Select Case X1
            Case X2
                For j = 1 To ArX.Length
                    If ArX(j - 1) = X1 Then
                        c = ArY(j - 1)
                    End If
                Next
            Case Else
                For j = 1 To ArX.Length
                    If ArX(j - 1) = X1 Then
                        f1 = ArY(j - 1)
                    ElseIf ArX(j - 1) = X2 Then
                        f2 = ArY(j - 1)
                    End If
                Next
                c = f1 + (X - X1) * ((f2 - f1) / (X2 - X1))
        End Select
        Return c
    End Function

    Public Shared Function deg2rad(Alpha As Double)
        Dim rad As Double = Math.PI * Alpha / 180
        Return rad
    End Function

    Public Shared Function rad2deg(Alpha As Double)
        Dim deg As Double = Alpha * 180 / Math.PI
        Return deg
    End Function

    Public Shared Function polyfit2(X() As Double, Y() As Double) As (a1 As Double, b1 As Double, c1 As Double)

        '' Curve Fitting for HERMES V 
        '' let there be N couples of values x And y. Here N=10;

        'X values (e.g. AoA / Re)
        'X(1) = -2 ; 
        'X(2) = -1 ; 
        'X(3) = 0 ; 
        'X(4) = 1 ; 
        'X(5) = 2 ;
        'X(6) = 3;
        'X(7) = 4;
        'X(8) = 5;
        'X(9) = 6;
        'X(10) = 7;

        'Y values (e.g. Cl)
        'Y(1) = 0.45;
        'Y(2) = 0.42;
        'Y(3) = 0.43 ;
        'Y(4) = 0.46 ;
        'Y(5) = 0.41 ;
        'Y(6) = 0.39; 
        'Y(7) = 0.4 ;
        'Y(8) = 0.37 ;
        'Y(9) = 0.35;
        'Y(10) = 0.3; 

        ' Get N value as the length of the matrices
        Dim N As Integer = X.Length

        Dim sumx0 As Double = N
        Dim sumy0 As Double = 0
        Dim sumx1 As Double = 0
        Dim sumy1 As Double = 0
        Dim sumx2 As Double = 0
        Dim sumy2 As Double = 0
        Dim sumx3 As Double = 0
        Dim sumx4 As Double = 0
        For i = 0 To N - 1 Step 1
            sumy0 += Y(i)
            sumx1 += X(i)
            sumy1 += Y(i) * X(i)
            sumx2 += X(i) * X(i)
            sumy2 += Y(i) * X(i) * X(i)
            sumx3 += X(i) * X(i) * X(i)
            sumx4 += X(i) * X(i) * X(i) * X(i)
        Next

        Dim A = New Double(2, 2) {{sumx0, sumx1, sumx2}, {sumx1, sumx2, sumx3}, {sumx2, sumx3, sumx4}}

        Dim M0 = New Double(2, 2) {{sumy0, sumx1, sumx2}, {sumy1, sumx2, sumx3}, {sumy2, sumx3, sumx4}}

        Dim M1 = New Double(2, 2) {{sumx0, sumy0, sumx2}, {sumx1, sumy1, sumx3}, {sumx2, sumy2, sumx4}}

        Dim M2 = New Double(2, 2) {{sumx0, sumx1, sumy0}, {sumx1, sumx2, sumy1}, {sumx2, sumx3, sumy2}}


        Dim detM0 As Double = M0(0, 0) * (M0(1, 1) * M0(2, 2) - M0(2, 1) * M0(1, 2)) - M0(0, 1) * (M0(1, 0) * M0(2, 2) - M0(2, 0) * M0(1, 2)) + M0(0, 2) * (M0(1, 0) * M0(2, 1) - M0(2, 0) * M0(1, 1))
        Dim detM1 As Double = M1(0, 0) * (M1(1, 1) * M1(2, 2) - M1(2, 1) * M1(1, 2)) - M1(0, 1) * (M1(1, 0) * M1(2, 2) - M1(2, 0) * M1(1, 2)) + M1(0, 2) * (M1(1, 0) * M1(2, 1) - M1(2, 0) * M1(1, 1))
        Dim detM2 As Double = M2(0, 0) * (M2(1, 1) * M2(2, 2) - M2(2, 1) * M2(1, 2)) - M2(0, 1) * (M2(1, 0) * M2(2, 2) - M2(2, 0) * M2(1, 2)) + M2(0, 2) * (M2(1, 0) * M2(2, 1) - M2(2, 0) * M2(1, 1))
        Dim detA As Double = A(0, 0) * (A(1, 1) * A(2, 2) - A(2, 1) * A(1, 2)) - A(0, 1) * (A(1, 0) * A(2, 2) - A(2, 0) * A(1, 2)) + A(0, 2) * (A(1, 0) * A(2, 1) - A(2, 0) * A(1, 1))

        '% Get a, b, c, for a*x^2 + b*x + c
        Dim c1 As Double = detM0 / detA
        Dim b1 As Double = detM1 / detA
        Dim a1 As Double = detM2 / detA
        Return (a1, b1, c1)




    End Function

End Class