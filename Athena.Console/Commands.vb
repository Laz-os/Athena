

Public Module Commands

    Public Structure CalculationCommand

        Public Shared Sub test()
            TestAerodynamicSolver()
        End Sub
        Public Shared Sub test_hamming()
            TestHammingSolver()
        End Sub
        Public Shared Sub mkl_on()
            DotNumerics.LinearAlgebra.LinearEquations.UseIntelMathKernel = True
        End Sub
        Public Shared Sub mkl_off()
            DotNumerics.LinearAlgebra.LinearEquations.UseIntelMathKernel = False
        End Sub
        Public Shared Sub mkl_test()
            DotNumerics.LinearAlgebra.IntelMathKernelTest.Start()
        End Sub
        Public Shared Sub mkl_status()
            Dim status As Boolean = DotNumerics.LinearAlgebra.LinearEquations.UseIntelMathKernel
        End Sub
        Public Shared Sub mkl_path(path As String)
            MklSetup.ChangePath(path)
        End Sub

        Public Shared Sub set_velocity(Vx As Double, Vy As Double, Vz As Double)
            MyProjectRoot.SimulationSettings.StreamVelocity.X = Vx
            MyProjectRoot.SimulationSettings.StreamVelocity.Y = Vy
            MyProjectRoot.SimulationSettings.StreamVelocity.Z = Vz
        End Sub
        'Public Sub set_omega(OmX As Double, OmY As Double, OmZ As Double)
        '    MyProjectRoot.SimulationSettings.StreamRotation.X = OmX
        '    MyProjectRoot.SimulationSettings.StreamRotation.Y = OmY
        '    MyProjectRoot.SimulationSettings.StreamRotation.Z = OmZ
        'End Sub
        Public Shared Sub set_alfa(aoa As Double)
            Dim Alfa As Double = aoa / 180 * Math.PI
            Dim V As Double = MyProjectRoot.SimulationSettings.StreamVelocity.Norm2
            MyProjectRoot.SimulationSettings.StreamVelocity.X = V * Math.Cos(Alfa)
            MyProjectRoot.SimulationSettings.StreamVelocity.Y = 0
            MyProjectRoot.SimulationSettings.StreamVelocity.Z = V * Math.Sin(Alfa)
        End Sub
        'Public Shared Sub set_delta()

        '    Dim SurfaceName As String
        '    Dim RegionIndex As Integer
        '    Dim Delta As Double = 0 / 180 * Math.PI

        '    ' Find the lifting surface
        '    '-----------------------------------------------------------------

        '    Dim LiftingSurface As LiftingSurface = Nothing

        '    For Each Surface As Surface In MyProjectRoot.Model.Objects

        '        If Surface.Name.ToLower = SurfaceName.ToLower Then

        '            If TypeOf (Surface) Is LiftingSurface Then
        '                LiftingSurface = Surface
        '            End If

        '        End If

        '    Next

        '    If LiftingSurface Is Nothing Then

        '    Else

        '        ' Check the region and flap
        '        '-----------------------------------------------------------------

        '        If RegionIndex < 1 Or RegionIndex > LiftingSurface.WingRegions.Count Then

        '        Else

        '            Dim Region As WingRegion = LiftingSurface.WingRegions(RegionIndex - 1)

        '            If Region.Flapped Then
        '                Region.FlapDeflection = Delta
        '                LiftingSurface.GenerateMesh()



        '            End If

        '        End If

        '    End If


        'End Sub
        Public Shared Sub set_altitude(alt As Double)
            MyProjectRoot.SimulationSettings.AssignStandardAtmosphere(alt)
        End Sub
        'Public Shared Sub server()
        '    RunServer()
        'End Sub

    End Structure

    'Private OutputFile As StreamWriter

    'Private Sub WriteToFile(Line As String)
    '    If OutputFile IsNot Nothing Then
    '        OutputFile.WriteLine(Line)
    '    End If
    'End Sub

    'Private Sub OutputConsoleMessage(Message As String)
    '    System.Console.WriteLine(Message)
    'End Sub

    'Private Sub OutputConsoleProgress(Title As String, Value As Integer)
    '    System.Console.WriteLine(Title)
    'End Sub

End Module
