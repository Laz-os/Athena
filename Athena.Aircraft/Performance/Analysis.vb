Imports Athena.Console
Imports OpenVOGEL.AeroTools.CalculationModel.Settings
Imports OpenVOGEL.AeroTools.CalculationModel.Solver
Imports OpenVOGEL.MathTools.Algebra.EuclideanSpace
Imports Athena.Aircraft.Models.Design
Imports Athena.Aircraft.Models.Design.Aerodynamics
Imports Athena.Functions.Functions
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components.Basics

Namespace Performance
    Public Module Analysis

        Public Sub Steady(ByRef CalculationCore As Solver)
            MyProjectRoot.StartCalculation(CalculationType.SteadyState, CalculationCore)
        End Sub
        Public Sub free_flight(ByRef CalculationCore As Solver)
            MyProjectRoot.StartCalculation(CalculationType.FreeFlight, CalculationCore)
        End Sub
        Public Sub aeroelastic(ByRef CalculationCore As Solver)
            MyProjectRoot.StartCalculation(CalculationType.Aeroelastic, CalculationCore)
        End Sub

        Public Sub DragPolar(altitude As Double, mach As Double)
            Commands.CalculationCommand.set_altitude(altitude)
            Dim ISA As New StandardAtmosphere(altitude)
            Dim Vel As Double = mach * ISA.SoundSpeed

            Dim Re As Double = Vel * Lmac / ISA.KinematicVisc

            Aircraft.Models.Design.Include_Fuselage = True
            For Each Surface As Surface In Model.Objects
                If Surface.Id = Models.Design.FuselageID Then
                    Surface.IncludeInCalculation = True
                End If
            Next

            AlfaScan(-3, 10.6, 1.5, Vel, Re)

        End Sub

        ''' Performs a series of steady analysis between Alfa1 and Alfa2 using the AlfaStep in between.
        Public Sub AlfaScan(Alfa1 As Double,
                                Alfa2 As Double,
                                AlfaS As Double, V As Double, Reynolds As Double)

            If Alfa2 < Alfa1 Then
                System.Console.WriteLine("the first angle must be smaller than the second one")
                Exit Sub
            End If

            Dim N As Integer = (Alfa2 - Alfa1) / AlfaS
            Dim first_run As Boolean = True
            For I = 0 To N
                Try
                    Dim Alfa = deg2rad(Math.Min(Alfa1 + I * AlfaS, Alfa2))
                    Commands.CalculationCommand.set_velocity(V * Math.Cos(Alfa), 0, V * Math.Sin(Alfa))

                    Dim aeroforces As New AeroForces
                    Dim solver As New Solver
                    Dim xx0 As Double = -15
                    Dim Aero As New Aerodynamics
                    Aero.SetTailIncidence(xx0, MyProjectRoot.Model)
                    Steady(solver)
                    aeroforces.Calculate(solver)
                    Dim g0 As Double = StaticMargin(0.2, 0, aeroforces.R.X)

                    If first_run Then
                        Dim Area = Athena.Aircraft.Models.Design.Structural.SurfaceArea(solver)
                        WingArea = Area.Sw
                        ElevatorArea = Area.Se
                        RudderArea = Area.Sr
                        FuselageArea = Area.Sfus
                        Aircraft.Models.Design.Include_Fuselage = False
                        For Each Surface As Surface In Model.Objects
                            If Surface.Id = Models.Design.FuselageID Then
                                Surface.IncludeInCalculation = False
                            End If
                        Next
                        I += -1
                        first_run = False
                        Continue For
                    End If

                    Dim aeroforces1 As New AeroForces
                    Dim solver1 As New Solver
                    Dim xx1 As Double = 15
                    Aero = New Aerodynamics
                    Aero.SetTailIncidence(xx1, MyProjectRoot.Model)
                    Steady(solver1)
                    aeroforces1.Calculate(solver1)
                    Dim g1 As Double = StaticMargin(0.2, 0, aeroforces1.R.X)

                    Dim root As Double = xx1 - g1 * (xx1 - xx0) / (g1 - g0)
                    Aero = New Aerodynamics
                    Dim CheckDef = Aero.CheckDeflection(root, g0, g1)
                    If CheckDef.Cancel Then
                        Continue For
                    End If
                    While Math.Abs(CheckDef.defl) > 0.01
                        xx0 = xx1
                        xx1 = root
                        g0 = g1
                        g1 = CheckDef.defl
                        root = xx1 - g1 * (xx1 - xx0) / (g1 - g0)
                        Aero = New Aerodynamics
                        CheckDef = Aero.CheckDeflection(root, g0, g1)
                        If CheckDef.Cancel Then
                            Continue For
                        End If
                    End While
                    CheckDef.Forces.Re = Reynolds
                    CheckDef.Forces.Deflection = root


                    Loads.Add(CheckDef.Forces)

                Catch exc As Exception
                    Continue For
                End Try

            Next

        End Sub

        '' Scans the airloads for a given set of flap deflections.
        'Public Sub DeltaScan(Alfa As Double,
        '                         SurfaceName As String,
        '                         RegionIndex As Integer,
        '                         Delta1 As Double,
        '                         Delta2 As Double,
        '                         DeltaS As Double)

        '    ' Find the lifting surface
        '    '-----------------------------------------------------------------

        '    Dim LiftingSurface As LiftingSurface = Nothing

        '    For Each Surface As Surface In Model.Objects

        '        If Surface.Name.ToLower = SurfaceName.ToLower Then

        '            If TypeOf (Surface) Is LiftingSurface Then

        '                LiftingSurface = Surface

        '            Else
        '                System.Console.WriteLine("the target surface exist in the model, but it is not a lifting surface")
        '                Exit Sub

        '            End If

        '        End If

        '    Next

        '    If LiftingSurface Is Nothing Then
        '        System.Console.WriteLine("the target surface does not exist in the model")
        '        Exit Sub
        '    End If

        '    ' Check the region and flap
        '    '-----------------------------------------------------------------

        '    If RegionIndex < 1 Or RegionIndex > LiftingSurface.WingRegions.Count Then
        '        System.Console.WriteLine(String.Format("invalid target region (must be between 1 and {0})", LiftingSurface.WingRegions.Count))
        '        Exit Sub
        '    End If

        '    Dim Region As WingRegion = LiftingSurface.WingRegions(RegionIndex - 1)

        '    If Not Region.Flapped Then
        '        System.Console.WriteLine("invalid target region (not flapped)")
        '        Exit Sub
        '    End If

        '    Dim OriginalDeflection As Double = Region.FlapDeflection

        '    ' Check the given angles
        '    '-----------------------------------------------------------------

        '    If Delta2 < Delta1 Then
        '        System.Console.WriteLine("the first angle must be smaller than the second one")
        '        Exit Sub
        '    End If

        '    Dim N As Integer = (Delta2 - Delta1) / DeltaS
        '    Dim Loads As New List(Of AirLoads)

        '    Dim V As Double = ProjectRoot.SimulationSettings.
        '    .EuclideanNorm

        '    ' Set the incidence angle
        '    '-----------------------------------------------------------------

        '    ProjectRoot.SimulationSettings.StreamVelocity.X = V * Math.Cos(Alfa / 180.0 * Math.PI)
        '    ProjectRoot.SimulationSettings.StreamVelocity.Z = V * Math.Sin(Alfa / 180.0 * Math.PI)

        '    ' Scan the flap deflection
        '    '-----------------------------------------------------------------

        '    For I = 0 To N

        '        System.Console.WriteLine(String.Format("STEP {0} of {1}", I, N))

        '        Region.FlapDeflection = Math.PI * Math.Min(Delta1 + I * DeltaS, Delta2) / 180.0

        '        LiftingSurface.GenerateMesh()

        '        Dim Kernel As New Solver.Solver

        '        ProjectRoot.StartCalculation(CalculationType.SteadyState, Kernel)

        '        Loads.Add(Kernel.GlobalAirloads)

        '    Next

        '    Region.FlapDeflection = OriginalDeflection

        '    ' Write results
        '    '-----------------------------------------------------------------

        '    Dim FileId As Integer = FreeFile()

        '    FileOpen(FileId, Path.Combine(Path.GetDirectoryName(FilePath), Path.GetFileNameWithoutExtension(FilePath)) & "_batch.dat", OpenMode.Output)

        '    PrintLine(FileId, "OpenVOGEL delta scan")
        '    PrintLine(FileId, "Kernel version: " & Solver.Solver.Version)
        '    PrintLine(FileId, "Original model: " & ProjectRoot.FilePath)
        '    PrintLine(FileId, "")

        '    PrintLine(FileId, String.Format("L = {0,12:E6}m", Loads(0).Length))
        '    PrintLine(FileId, String.Format("A = {0,12:E6}m²", Loads(0).Area))
        '    PrintLine(FileId, String.Format("q = {0,12:E6}Pa", Loads(0).DynamicPressure))
        '    PrintLine(FileId, String.Format("a = {0,12:E6}°", Loads(0).Alfa))

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force coefficients")
        '    PrintLine(FileId, String.Format("{0,-6} {1,-14} {2,-14} {3,-14}", "Delta", "CL", "CDi", "CDp"))

        '    Dim J = 0

        '    For Each Load In Loads

        '        Dim Delta = Math.Min((Delta1 + J * DeltaS), Delta2)

        '        PrintLine(FileId, String.Format("{0,6:F2} {1,14:E6} {2,14:E6} {3,14:E6}",
        '                                            Delta,
        '                                            Load.LiftCoefficient,
        '                                            Load.InducedDragCoefficient,
        '                                            Load.SkinDragCoefficient))

        '        J += 1
        '    Next

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force and moment coefficients")
        '    PrintLine(FileId, String.Format("{0,-6} {1,-14} {2,-14} {3,-14} {4,-14} {5,-14} {6,-14}", "Delta", "Fx", "Fy", "Fz", "Mx", "My", "Mz"))

        '    J = 0

        '    For Each Load In Loads

        '        Dim Delta = Math.Min((Delta1 + J * DeltaS), Delta2)

        '        Dim qS As Double = Load.DynamicPressure * Load.Area
        '        Dim qSL As Double = Load.DynamicPressure * Load.Area * Load.Length

        '        PrintLine(FileId, String.Format("{0,6:F3} {1,14:E6} {2,14:E6} {3,14:E6} {4,14:E6} {5,14:E6} {6,14:E6}",
        '                                            Delta,
        '                                            Load.Force.X / qS,
        '                                            Load.Force.Y / qS,
        '                                            Load.Force.Z / qS,
        '                                            Load.Moment.X / qSL,
        '                                            Load.Moment.Y / qSL,
        '                                            Load.Moment.Z / qSL))

        '        J += 1
        '    Next

        '    FileClose(FileId)

        'End Sub

        '' Scans the airloads for a given set of flap deflections.
        'Public Sub AlfaDeltaScan(Alfa1 As Double,
        '                             Alfa2 As Double,
        '                             AlfaS As Double,
        '                             SurfaceName As String,
        '                             RegionIndex As Integer,
        '                             Delta1 As Double,
        '                             Delta2 As Double,
        '                             DeltaS As Double)

        '    ' Find the lifting surface
        '    '-----------------------------------------------------------------

        '    Dim LiftingSurface As LiftingSurface = Nothing

        '    For Each Surface As Surface In Model.Objects

        '        If Surface.Name.ToLower = SurfaceName.ToLower Then

        '            If TypeOf (Surface) Is LiftingSurface Then

        '                LiftingSurface = Surface

        '            Else
        '                System.Console.WriteLine("the target surface exist in the model, but it is not a lifting surface")
        '                Exit Sub

        '            End If

        '        End If

        '    Next

        '    If LiftingSurface Is Nothing Then
        '        System.Console.WriteLine("the target surface does not exist in the model")
        '        Exit Sub
        '    End If

        '    ' Check the region and flap
        '    '-----------------------------------------------------------------

        '    If RegionIndex < 1 Or RegionIndex > LiftingSurface.WingRegions.Count Then
        '        System.Console.WriteLine(String.Format("invalid target region (must be between 1 and {0})", LiftingSurface.WingRegions.Count))
        '        Exit Sub
        '    End If

        '    Dim Region As WingRegion = LiftingSurface.WingRegions(RegionIndex - 1)

        '    If Not Region.Flapped Then
        '        System.Console.WriteLine("invalid target region (not flapped)")
        '        Exit Sub
        '    End If

        '    Dim OriginalDeflection As Double = Region.FlapDeflection

        '    ' Check the given angles
        '    '-----------------------------------------------------------------

        '    If Alfa2 < Alfa1 Then
        '        System.Console.WriteLine("the first incidence angle must be smaller than the second one")
        '        Exit Sub
        '    End If

        '    If Delta2 < Delta1 Then
        '        System.Console.WriteLine("the first deflection angle must be smaller than the second one")
        '        Exit Sub
        '    End If

        '    Dim Na As Integer = (Alfa2 - Alfa1) / AlfaS
        '    Dim Nd As Integer = (Delta2 - Delta1) / DeltaS
        '    Dim Loads As New List(Of AirLoads)

        '    Dim V As Double = ProjectRoot.SimulationSettings.StreamVelocity.EuclideanNorm

        '    For I = 0 To Na

        '        System.Console.WriteLine(String.Format("ALFA STEP {0} of {1}", I, Na))

        '        ' Set the incidence angle
        '        '-----------------------------------------------------------------

        '        Dim Alfa = Math.PI * Math.Min(Alfa1 + I * AlfaS, Alfa2) / 180.0

        '        ProjectRoot.SimulationSettings.StreamVelocity.X = V * Math.Cos(Alfa)
        '        ProjectRoot.SimulationSettings.StreamVelocity.Z = V * Math.Sin(Alfa)

        '        ' Scan the flap deflection
        '        '-----------------------------------------------------------------

        '        For J = 0 To Nd

        '            System.Console.WriteLine(String.Format("DELTA STEP {0} of {1}", J, Nd))

        '            Region.FlapDeflection = Math.PI * Math.Min(Delta1 + J * DeltaS, Delta2) / 180.0

        '            LiftingSurface.GenerateMesh()

        '            Dim Kernel As New Solver.Solver

        '            ProjectRoot.StartCalculation(CalculationType.SteadyState, Kernel)

        '            Loads.Add(Kernel.GlobalAirloads)

        '        Next

        '        Region.FlapDeflection = OriginalDeflection

        '    Next

        '    ' Write results in dat file
        '    '-----------------------------------------------------------------

        '    Dim FileId As Integer = FreeFile()
        '    FileOpen(FileId, Path.Combine(Path.GetDirectoryName(FilePath), Path.GetFileNameWithoutExtension(FilePath)) & "_batch.dat", OpenMode.Output)

        '    PrintLine(FileId, "OpenVOGEL alfa delta scan")
        '    PrintLine(FileId, "Kernel version: " & Solver.Solver.Version)
        '    PrintLine(FileId, "Original model: " & ProjectRoot.FilePath)
        '    PrintLine(FileId, "")

        '    PrintLine(FileId, String.Format("L = {0,12:E6}m", Loads(0).Length))
        '    PrintLine(FileId, String.Format("A = {0,12:E6}m²", Loads(0).Area))
        '    PrintLine(FileId, String.Format("q = {0,12:E6}Pa", Loads(0).DynamicPressure))
        '    PrintLine(FileId, String.Format("a = {0,12:E6}°", Loads(0).Alfa))

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force coefficients")
        '    PrintLine(FileId, String.Format("{0,-6} {1,-6} {2,-14} {3,-14} {4,-14} {5,-14}", "Alfa", "Delta", "CL", "CDi", "CDp", "Xcg"))

        '    Dim L = 0
        '    Dim K = 0

        '    For Each Load In Loads

        '        Dim Alfa = Math.Min((Alfa1 + L * AlfaS), Alfa2)

        '        Dim Delta = Math.Min((Delta1 + K * DeltaS), Delta2)

        '        Dim qSL As Double = Load.DynamicPressure * Load.Area * Load.Length

        '        Dim Xcg As Double = -(Load.Moment.Y / qSL) / Load.LiftCoefficient

        '        PrintLine(FileId, String.Format("{0,6:F2} {1,6:F2} {2,14:E6} {3,14:E6} {4,14:E6} {5,14:E6}",
        '                                            Alfa,
        '                                            Delta,
        '                                            Load.LiftCoefficient,
        '                                            Load.InducedDragCoefficient,
        '                                            Load.SkinDragCoefficient,
        '                                            Xcg))

        '        If K = Nd Then
        '            K = 0
        '            L += 1
        '        Else
        '            K += 1
        '        End If

        '    Next

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force and moment coefficients")
        '    PrintLine(FileId, String.Format("{0,-6} {1,-6} {2,-14} {3,-14} {4,-14} {5,-14} {6,-14} {7,-14}", "Alfa", "Delta", "Fx", "Fy", "Fz", "Mx", "My", "Mz"))

        '    L = 0
        '    K = 0

        '    For Each Load In Loads

        '        Dim Alfa = Math.Min((Alfa1 + L * AlfaS), Alfa2)

        '        Dim Delta = Math.Min((Delta1 + K * DeltaS), Delta2)

        '        Dim qS As Double = Load.DynamicPressure * Load.Area
        '        Dim qSL As Double = Load.DynamicPressure * Load.Area * Load.Length

        '        PrintLine(FileId, String.Format("{0,6:F2} {1,6:F2} {2,14:E6} {3,14:E6} {4,14:E6} {5,14:E6} {6,14:E6} {7,14:E6}",
        '                                            Alfa,
        '                                            Delta,
        '                                            Load.Force.X / qS,
        '                                            Load.Force.Y / qS,
        '                                            Load.Force.Z / qS,
        '                                            Load.Moment.X / qSL,
        '                                            Load.Moment.Y / qSL,
        '                                            Load.Moment.Z / qSL))

        '        If K = Nd Then
        '            K = 0
        '            L += 1
        '        Else
        '            K += 1
        '        End If

        '    Next

        '    FileClose(FileId)

        '    ' Write results in Scilab script file to plot the equilibrium states
        '    '-------------------------------------------------------------------

        '    FileId = FreeFile()
        '    FileOpen(FileId, Path.Combine(Path.GetDirectoryName(FilePath), Path.GetFileNameWithoutExtension(FilePath)) & "_script.sce", OpenMode.Output)
        '    PrintLine(FileId, "// OpenVOGEL automatic script for alfa-delta scan")
        '    PrintLine(FileId, "// Kernel version: " & Solver.Solver.Version)
        '    PrintLine(FileId, "// Original model: " & ProjectRoot.FilePath)
        '    PrintLine(FileId, "")

        '    Dim Line As String = ""

        '    ' Alfa and delta vectors
        '    '----------------------------------------------------------------

        '    For I = 0 To Na

        '        Dim Alfa = Math.Min((Alfa1 + I * AlfaS), Alfa2)

        '        Line = Line & String.Format("{0,8:F2}", Alfa)

        '    Next

        '    PrintLine(FileId, String.Format("X = [{0}]", Line))

        '    Line = ""

        '    For I = 0 To Nd

        '        Dim Delta = Math.Min((Delta1 + I * DeltaS), Delta2)

        '        Line = Line & String.Format("{0,8:F2}", Delta)

        '    Next

        '    PrintLine(FileId, String.Format("Y = [{0}]", Line))

        '    ' Lift coefficient
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "CL = [")

        '    K = 0
        '    Line = ""

        '    For Each Load In Loads

        '        Line = Line & String.Format(" {0,14:E6}", Load.LiftCoefficient)

        '        If K = Nd Then
        '            PrintLine(FileId, Line)
        '            Line = ""
        '            K = 0
        '        Else
        '            K += 1
        '        End If

        '    Next

        '    PrintLine(FileId, "]")

        '    ' Vertical force coefficient
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "CFz = [")

        '    K = 0
        '    Line = ""

        '    For Each Load In Loads

        '        Dim qS As Double = Load.DynamicPressure * Load.Area

        '        Line = Line & String.Format(" {0,14:E6}", Load.Force.Z / qS)

        '        If K = Nd Then
        '            PrintLine(FileId, Line)
        '            Line = ""
        '            K = 0
        '        Else
        '            K += 1
        '        End If

        '    Next

        '    PrintLine(FileId, "]")

        '    ' Vertical force coefficient
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "CMy = [")

        '    K = 0
        '    Line = ""

        '    For Each Load In Loads

        '        Dim qSL As Double = Load.DynamicPressure * Load.Area * Load.Length

        '        Line = Line & String.Format(" {0,14:E6}", Load.Moment.Y / qSL)

        '        If K = Nd Then
        '            PrintLine(FileId, Line)
        '            Line = ""
        '            K = 0
        '        Else
        '            K += 1
        '        End If

        '    Next

        '    PrintLine(FileId, "]")

        '    PrintLine(FileId, "clf")
        '    PrintLine(FileId, "title(""Stability plot"", ""fontsize"", 4)")
        '    PrintLine(FileId, "xlabel(""alpha [degrees]"", ""fontsize"", 3)")
        '    PrintLine(FileId, "ylabel(""delta [degrees]"", ""fontsize"", 3)")
        '    PrintLine(FileId, "xgrid(3)")
        '    PrintLine(FileId, "legends([""iso-CL"", ""iso-Xcg""], [2, 5], ""lr"")")

        '    ' Plot lift contourn lines
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "// CL countour lines")
        '    PrintLine(FileId, "N_CL = 30")
        '    PrintLine(FileId, "Stl_CL = 2 * ones(1, N_CL)")
        '    PrintLine(FileId, "contour(X, Y, CL, N_CL, Stl_CL)")

        '    ' Plot the contour lines for the X coordinate of the 
        '    ' gravity center (Xcg)
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "// Expand CL And CM To refine Xcg")

        '    ' Build spline interpolation for CFz and FMy

        '    PrintLine(FileId, "CFz_Spline = splin2d(X, Y, CFz)")
        '    PrintLine(FileId, "CMy_Spline = splin2d(X, Y, CMy)")

        '    ' Expand the data in a finer grid

        '    PrintLine(FileId, String.Format("X_Int = linspace({0,6:F2}, {1,6:F2}, 100)", Alfa1, Alfa2))
        '    PrintLine(FileId, String.Format("Y_Int = linspace({0,6:F2}, {1,6:F2}, 100)", Delta1, Delta2))
        '    PrintLine(FileId, "[X_Grid,Y_Grid] = ndgrid(X_Int, Y_Int)")
        '    PrintLine(FileId, "CFz_Int = interp2d(X_Grid, Y_Grid, X, Y, CFz_Spline)")
        '    PrintLine(FileId, "CMy_Int = interp2d(X_Grid, Y_Grid, X, Y, CMy_Spline)")

        '    ' Compute center of gravity for the refined grid and plot the iso-curves

        '    PrintLine(FileId, "Xcg_Int = - CMy_Int ./ CFz_Int")

        '    PrintLine(FileId, "N_Xcg = 45")
        '    PrintLine(FileId, "Stl_Xcg = 5 * ones(1, N_Xcg)")
        '    PrintLine(FileId, "contour(X_Int, Y_Int, Xcg_Int, 45, Stl_Xcg)")

        '    FileClose(FileId)

        'End Sub

        '' Performs a series of steady analysis between Omega1 and Omega2 using the OmegaS in between.
        'Public Sub OmegaScan(OmegaMax As Double, No As Integer, Mass1 As Double, Mass2 As Double, Nm As Integer)

        '    Dim Loads As New List(Of AirLoads)

        '    ProjectRoot.SimulationSettings.ExtendWakes = False

        '    For I = 0 To No

        '        System.Console.WriteLine(String.Format("STEP {0} of {1}", I, No))

        '        ProjectRoot.SimulationSettings.StreamRotation.Y = -OmegaMax * I / No

        '        Dim Kernel As New Solver.Solver

        '        ProjectRoot.StartCalculation(CalculationType.SteadyState, Kernel)

        '        Loads.Add(Kernel.GlobalAirloads)

        '    Next

        '    Dim FileId As Integer = FreeFile()

        '    FileOpen(FileId, Path.Combine(Path.GetDirectoryName(FilePath), Path.GetFileNameWithoutExtension(FilePath)) & "_batch.dat", OpenMode.Output)

        '    PrintLine(FileId, "OpenVOGEL omega scan")
        '    PrintLine(FileId, "Kernel version: " & Solver.Solver.Version)
        '    PrintLine(FileId, "Original model: " & ProjectRoot.FilePath)
        '    PrintLine(FileId, "")

        '    PrintLine(FileId, String.Format("L = {0,12:E6}m", Loads(0).Length))
        '    PrintLine(FileId, String.Format("A = {0,12:E6}m²", Loads(0).Area))
        '    PrintLine(FileId, String.Format("q = {0,12:E6}Pa", Loads(0).DynamicPressure))
        '    PrintLine(FileId, String.Format("a = {0,12:E6}deg", Loads(0).Alfa * 180 / Math.PI))

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force coefficients")
        '    PrintLine(FileId, String.Format("{0,-8} {1,-14} {2,-14} {3,-14}", "Omega", "CL", "CDi", "CDp"))

        '    Dim J As Integer = 0

        '    For Each Load In Loads

        '        Dim Omega = -OmegaMax * J / No

        '        PrintLine(FileId, String.Format("{0,8:F4} {1,14:E6} {2,14:E6} {3,14:E6}",
        '                                            Omega,
        '                                            Load.LiftCoefficient,
        '                                            Load.InducedDragCoefficient,
        '                                            Load.SkinDragCoefficient))

        '        J += 1

        '    Next

        '    PrintLine(FileId, "")
        '    PrintLine(FileId, "# Force and moment coefficients")
        '    PrintLine(FileId, String.Format("{0,-8} {1,-14} {2,-14} {3,-14} {4,-14} {5,-14} {6,-14}", "Alfa", "Fx", "Fy", "Fz", "Mx", "My", "Mz"))

        '    J = 0

        '    For Each Load In Loads

        '        Dim Omega = -OmegaMax * J / No

        '        Dim qS As Double = Load.DynamicPressure * Load.Area
        '        Dim qSL As Double = Load.DynamicPressure * Load.Area * Load.Length

        '        PrintLine(FileId, String.Format("{0,8:F4} {1,14:E6} {2,14:E6} {3,14:E6} {4,14:E6} {5,14:E6} {6,14:E6}",
        '                                            Omega,
        '                                            Load.Force.X / qS,
        '                                            Load.Force.Y / qS,
        '                                            Load.Force.Z / qS,
        '                                            Load.Moment.X / qSL,
        '                                            Load.Moment.Y / qSL,
        '                                            Load.Moment.Z / qSL))

        '        J += 1

        '    Next

        '    FileClose(FileId)

        '    ' Write results in Scilab script file to plot the equilibrium states
        '    '-------------------------------------------------------------------

        '    FileId = FreeFile()
        '    FileOpen(FileId, Path.Combine(Path.GetDirectoryName(FilePath), Path.GetFileNameWithoutExtension(FilePath)) & "_script.sce", OpenMode.Output)
        '    PrintLine(FileId, "// OpenVOGEL automatic script for omega scan")
        '    PrintLine(FileId, "// Kernel version: " & Solver.Solver.Version)
        '    PrintLine(FileId, "// Original model: " & ProjectRoot.FilePath)
        '    PrintLine(FileId, "")

        '    Dim Velocity As Double = ProjectRoot.SimulationSettings.StreamVelocity.EuclideanNorm
        '    Dim Density As Double = ProjectRoot.SimulationSettings.Density
        '    Dim Area As Double = Loads(0).Area

        '    ' M vector (mass, limited to Mcrit)
        '    '----------------------------------------------------------------

        '    J = 0

        '    For Each Load In Loads

        '        Dim Kappa = (OmegaMax * J / No) / Velocity
        '        Dim Mcrit = Mass2

        '        If Kappa > 0 Then
        '            Mcrit = 0.5 * Load.Area * Density * Load.LiftCoefficient / Kappa
        '            If Mass2 > Mcrit Then
        '                PrintLine(FileId, String.Format("// WARNING: the upper mass limit is constrained to {0,14:E6}kg for a curvature of {1,14:E6}!", Mcrit, Kappa))
        '                Mass2 = 0.9 * Mcrit
        '                PrintLine(FileId, String.Format("// The upper mass limit has been reduced to {0,14:E6}kg", Mass2))
        '                If Mass1 > Mass2 Then
        '                    Mass1 = 0.5 * Mass2
        '                    PrintLine(FileId, String.Format("// The lower mass limit has also been reduced to {0,14:E6}kg", Mass1))
        '                End If
        '                PrintLine(FileId, "// Either reduce the mass or the maximum path curvature")
        '            End If
        '        End If

        '        J += 1

        '    Next

        '    PrintLine(FileId, String.Format("M = linspace({0,14:E6}, {1,14:E6}, {2})", Mass1, Mass2, Nm))

        '    ' C vector (curvature of the trajectory)
        '    '----------------------------------------------------------------

        '    Dim Line As String = ""

        '    For I = 0 To No

        '        Dim Omega = OmegaMax * I / No

        '        Line = Line & String.Format("{0,14:E6}", Omega / Velocity)

        '    Next

        '    PrintLine(FileId, String.Format("C  = [{0}]", Line))

        '    ' X vector (Xcg)
        '    '----------------------------------------------------------------

        '    Line = ""

        '    For Each Load In Loads

        '        Line = Line & String.Format(" {0,14:E6}", -Load.Moment.Y / (Load.Force.Z * Load.Length))

        '    Next

        '    PrintLine(FileId, String.Format("X  = [{0}]", Line))

        '    ' CL vector
        '    '----------------------------------------------------------------

        '    Line = ""

        '    For Each Load In Loads

        '        Line = Line & String.Format(" {0,14:E6}", Load.LiftCoefficient)

        '    Next

        '    PrintLine(FileId, String.Format("CL = [{0}]", Line))

        '    ' Other data
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, String.Format("r = {0,14:E6}", Density))
        '    PrintLine(FileId, String.Format("S = {0,14:E6}", Area))
        '    PrintLine(FileId, "g = 9.8")

        '    ' Compute velocity and load factor for each C and M
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, "V = zeros(length(M), length(C))")
        '    PrintLine(FileId, "n = zeros(length(M), length(C))")
        '    PrintLine(FileId, "for I = 1: length(M)")
        '    PrintLine(FileId, "    for J = 1: length(C)")
        '    PrintLine(FileId, "        V(I, J) = sqrt(2 * M(I) * 9.8 / (r * S * CL(J) - 2 * C(J) * M(I)))")
        '    PrintLine(FileId, "        n(I, J) = 1 + C(J) * V(I, J) ^ 2 / 9.8")
        '    PrintLine(FileId, "    end")
        '    PrintLine(FileId, "end")
        '    PrintLine(FileId, "scf(2)")
        '    PrintLine(FileId, "clf")

        '    ' Make the diagram
        '    '----------------------------------------------------------------

        '    PrintLine(FileId, String.Format("title(""Recovery performance (alpha={0:N0}°)"", ""fontsize"", 4)", Loads(0).Alfa * 180 / Math.PI))
        '    PrintLine(FileId, "xlabel(""mass [kg]"", ""fontsize"", 3)")
        '    PrintLine(FileId, "ylabel(""Xcg [x/L]"", ""fontsize"", 3)")
        '    PrintLine(FileId, "xgrid(3)")
        '    PrintLine(FileId, "legends([""iso-n"", ""iso-V""], [2, 5], ""lr"")")
        '    PrintLine(FileId, "Stl_n = 2 * ones(1, 10)")
        '    PrintLine(FileId, "Stl_V = 5 * ones(1, 10)")
        '    PrintLine(FileId, "contour2d(M, X, n, 10, Stl_n)")
        '    PrintLine(FileId, "contour2d(M, X, V, 10, Stl_V)")

        '    FileClose(FileId)

        'End Sub

    End Module

    Public Class AeroForces
        Public Property Airloads As New OpenVOGEL.AeroTools.CalculationModel.Models.Aero.AirLoads
        Public Property AoA As Double
        Public Property Beta As Double
        Public Property Fx As Double
        Public Property Fy As Double
        Public Property Fz As Double
        Public Property Lift As Double
        Public Property Drag As Double
        Public Property Sideforce As Double
        Public Property Mx As Double
        Public Property My As Double
        Public Property Mz As Double
        Public Property R As New Vector3
        Public Property Re As Double
        Public Property Deflection As Double
        Public Sub Calculate(_CalculationCore As Solver)

            Dim TotalForce As New Vector3
            Dim TotalMoment As New Vector3
            Airloads.Clear()
            For i = 0 To _CalculationCore.Lattices.Count - 1

                Airloads.Add(_CalculationCore.Lattices(i).AirLoads)

            Next

            Dim q As Double = _CalculationCore.StreamDynamicPressure

            TotalForce.SetToCero()

            TotalForce.Add(Airloads.LiftForce)

            TotalForce.Add(Airloads.InducedDragForce)

            TotalForce.Add(Airloads.SkinDragForce)

            TotalForce.Add(Airloads.BodyForce)
            'TotalForce.Scale(q)

            TotalMoment.SetToCero()

            TotalMoment.Add(Airloads.LiftMoment)

            TotalMoment.Add(Airloads.InducedDragMoment)

            TotalMoment.Add(Airloads.SkinDragMoment)

            TotalMoment.Add(Airloads.BodyMoment)

            'TotalMoment.Scale(q)

            'declare the reference point for Moment calculation
            R.X = 0
            R.Y = 0
            R.Z = 0

            TotalMoment.AddCrossProduct(TotalForce, R)

#Region "Reference Point"
            Dim MomT As String

            If TotalMoment.Y < 0 Then
                MomT = "Negative"
            ElseIf TotalMoment.Y > 0 Then
                MomT = "Positive"
            End If

            Dim stp As Double = 1
            Dim dist As Double = 0
            For i As Double = 0 To 100 Step stp
                TotalMoment.SetToCero()
                TotalMoment.Add(Airloads.LiftMoment)
                TotalMoment.Add(Airloads.InducedDragMoment)
                TotalMoment.Add(Airloads.SkinDragMoment)
                TotalMoment.Add(Airloads.BodyMoment)
                R.X += stp '+ temp   
                TotalMoment.AddCrossProduct(TotalForce, R)
                Select Case MomT
                    Case "Negative"
                        If TotalMoment.Y < 0 Then
                            dist = R.X
                        ElseIf TotalMoment.Y > 0.001 Then
                            R.X = dist
                            stp /= 10
                        Else Exit For

                        End If
                    Case "Positive"
                        If TotalMoment.Y > 0 Then
                            dist = R.X
                        ElseIf TotalMoment.Y < 0.001 Then
                            R.X = dist
                            stp /= 10
                        Else Exit For

                        End If
                End Select
            Next
#End Region

            Fx = TotalForce.X
            Fy = TotalForce.Y
            Fz = TotalForce.Z

            Mx = TotalMoment.X
            My = TotalMoment.Y
            Mz = TotalMoment.Z

            ' Dimensionless

            Dim S As Double = Math.Max(0.001, 0) ''edw mporw na allaksw to miden me epifania anaforaw gia na exv adiastatous syntelestes

            Dim c As Double = Math.Max(0.001, 0)

            Dim qS As Double = q * S

            Dim qSc As Double = q * S * c

            ' Components in aerodynamic coordinates

            Dim Basis As New Base3

            Basis.U.X = _CalculationCore.StreamVelocity.X
            Basis.U.Y = _CalculationCore.StreamVelocity.Y
            Basis.U.Z = _CalculationCore.StreamVelocity.Z
            Basis.U.Normalize()

            Basis.W.X = Basis.U.X
            Basis.W.Z = Basis.U.Z
            Dim Ux As Double = Basis.W.X
            Dim Uz As Double = Basis.W.Z
            Basis.W.Z = Ux
            Basis.W.X = -Uz
            Basis.W.Normalize()

            Basis.V.FromVectorProduct(Basis.W, Basis.U)

            Lift = TotalForce.InnerProduct(Basis.W)
            Drag = TotalForce.InnerProduct(Basis.U)
            Sideforce = TotalForce.InnerProduct(Basis.V)

            AoA = (Math.Asin(_CalculationCore.StreamVelocity.Z / _CalculationCore.StreamVelocity.Norm2)) / Math.PI * 180
            Beta = Math.Asin(_CalculationCore.StreamVelocity.Y / _CalculationCore.StreamVelocity.Norm2) / Math.PI * 180
        End Sub
    End Class


End Namespace
