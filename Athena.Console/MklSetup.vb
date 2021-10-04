Public Module MklSetup

    ''' <summary>
    ''' The original content of the path enviromental varialbe
    ''' </summary>
    Private ReadOnly OriginalPath As String = Environment.GetEnvironmentVariable("path")

    ''' <summary>
    ''' The path to the MKL suit (mkl_rt.dll)
    ''' </summary>
    Private MklSuitPath As String = ""

    ''' <summary>
    ''' The path to the local file containing the last mkl path entered by the user
    ''' </summary>
    Private InitPath As String = IO.Path.Combine(IO.Directory.GetCurrentDirectory, "MklPath.dat")

    ''' <summary>
    ''' Sets the MKL library path
    ''' </summary>
    Public Sub ChangePath(Path As String)

        MklSuitPath = Path
        Environment.SetEnvironmentVariable("path", String.Format("{0};{1}", MklSuitPath, OriginalPath))

        Dim FileId As Integer = FreeFile()
        FileOpen(FileId, InitPath, OpenMode.Output)
        PrintLine(FileId, Path)
        FileClose(FileId)

    End Sub

    ''' <summary>
    ''' 
    ''' </summary>
    Public Sub Initialize()

        DotNumerics.LinearAlgebra.LinearEquations.UseIntelMathKernel = False

        ' Use the path previously given by the user (if the configuration exists)

        If IO.File.Exists(InitPath) Then
            Dim FileId As Integer = FreeFile()
            FileOpen(FileId, InitPath, OpenMode.Input)
            If Not EOF(FileId) Then
                MklSuitPath = LineInput(FileId)
                Environment.SetEnvironmentVariable("path", String.Format("{0};{1}", MklSuitPath, OriginalPath))
                DotNumerics.LinearAlgebra.LinearEquations.UseIntelMathKernel = True
            End If
            FileClose(FileId)
        End If

    End Sub


End Module
