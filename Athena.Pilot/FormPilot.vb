Imports Athena.Genetic.Genetic
Imports Athena.Console
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components.Basics
Imports OpenVOGEL.DesignTools.VisualModel.Models.Components
Imports Athena.Aircraft
Imports Athena.Aircraft.Models


Public Class FormPilot

    Private Sub FormPilot_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ''MklSetup.Initialize()

        MyProjectRoot.Initialize()

        'Dim dlgOpenFile As New OpenFileDialog()

        'dlgOpenFile.Filter = "Intel MKL directory (*.dll)|*.dll*"

        'Dim AcceptFile As MsgBoxResult = dlgOpenFile.ShowDialog()

        'If AcceptFile = MsgBoxResult.Ok Then

        '    CalculationCommand.mkl_path(My.Computer.FileSystem.GetParentPath(dlgOpenFile.FileName))

        'End If

        ''CalculationCommand.mkl_off()

    End Sub

    Private Sub OpenProject_Click(sender As Object, e As EventArgs) Handles OpenProject.Click
        MyProjectRoot.RestartProject()

        Dim dlgOpenFile As New OpenFileDialog()

        dlgOpenFile.Filter = "Vogel proyect files (*.vog)|*.vog"

        Dim AcceptFile As MsgBoxResult = dlgOpenFile.ShowDialog()

        If AcceptFile = MsgBoxResult.Ok Then

            MyProjectRoot.FilePath = dlgOpenFile.FileName

            MyProjectRoot.ReadFromXML()

        End If

    End Sub

    Public Sub Optimization_Click(sender As Object, e As EventArgs) Handles Optimization.Click

        'Commands.ProcessCommand("server", True)
        Dim Gen As New Genetic.Genetic
        Me.GetSettings(Gen)
        DefineLiftingSurfaces()
        Dim X2 = Gen.GenAl(AddressOf Pilot.StartCalculation, CheckBox10.Checked) 'second param defines whether the optimisation is contstrained
        'SetDesignVariables(X2, MyProjectRoot.Model, aero, False)
        MyProjectRoot.WriteToXML()
    End Sub
    Public Sub GetSettings(ByRef Gen As Genetic.Genetic)

        Gen.Ngen = NumericUpDown15.Value
        Gen.Npop = NumericUpDown16.Value
        Gen.Nelite = NumericUpDown17.Value
        Gen.Pmut = NumericUpDown18.Value
        Gen.Nvar = NumericUpDown19.Value
        Dim Vars As Integer = 0
        ReDim Gen.Xmin(Gen.Nvar - 1)
        ReDim Gen.Xmax(Gen.Nvar - 1)
        ReDim Gen.X2(Gen.Nvar - 1)
        'Populate Xmin, Xmax
        '-------------------
        If CheckBox1.Checked Then
            DesignParameters.Add(CheckBox1.Text)
            Gen.Xmin(Vars) = NumericUpDown1.Value
            Gen.Xmax(Vars) = NumericUpDown2.Value
            Vars += 1
        End If
        If CheckBox2.Checked Then
            DesignParameters.Add(CheckBox2.Text)
            Gen.Xmin(Vars) = NumericUpDown3.Value
            Gen.Xmax(Vars) = NumericUpDown4.Value
            Vars += 1
        End If
        If CheckBox3.Checked Then
            DesignParameters.Add(CheckBox3.Text)
            Gen.Xmin(Vars) = NumericUpDown5.Value
            Gen.Xmax(Vars) = NumericUpDown6.Value
            Vars += 1
        End If
        If CheckBox4.Checked Then
            DesignParameters.Add(CheckBox4.Text)
            Gen.Xmin(Vars) = NumericUpDown7.Value
            Gen.Xmax(Vars) = NumericUpDown8.Value
            Vars += 1
        End If
        If CheckBox5.Checked Then
            DesignParameters.Add(CheckBox5.Text)
            Gen.Xmin(Vars) = NumericUpDown9.Value
            Gen.Xmax(Vars) = NumericUpDown10.Value
            Vars += 1
        End If
        If CheckBox6.Checked Then
            DesignParameters.Add(CheckBox6.Text)
            Gen.Xmin(Vars) = NumericUpDown11.Value
            Gen.Xmax(Vars) = NumericUpDown12.Value
            Gen.AirfoilIndex = Vars
            Gen.AirfoilUse = True
            Vars += 1
        End If
        If CheckBox7.Checked Then
            DesignParameters.Add(CheckBox7.Text)
            Gen.Xmin(Vars) = NumericUpDown13.Value
            Gen.Xmax(Vars) = NumericUpDown14.Value
            Vars += 1
        End If
        If CheckBox8.Checked Then
            DesignParameters.Add(CheckBox8.Text)
            Gen.Xmin(Vars) = NumericUpDown20.Value
            Gen.Xmax(Vars) = NumericUpDown21.Value
            Vars += 1
        End If
        If CheckBox9.Checked Then
            DesignParameters.Add(CheckBox9.Text)
            Gen.Xmin(Vars) = NumericUpDown22.Value
            Gen.Xmax(Vars) = NumericUpDown23.Value
        End If
    End Sub

    ''' <summary>
    ''' Defines main wing by comparing the span of all lifting surfaces
    ''' Main wing is set to be that with max Span
    ''' </summary>
    Private Sub DefineLiftingSurfaces()
        Dim WingRnk(1) As Double
        Dim span(1) As Double
        Dim id(1) As Guid
        Dim index As Integer = 0
        Dim SurfaceRank As Integer = 0
        For Each Surface As Surface In Model.Objects
            If TypeOf Surface Is LiftingSurface Then
                Dim Wing As LiftingSurface = Surface
                For Each WingRegion As WingRegion In Wing.WingRegions

                    If WingRegion.Dihedral = 90 Then
                        Design.RudderID = Surface.Id
                        Design.RudderRank = SurfaceRank
                        SurfaceRank += 1
                    Else
                        id(index) = Surface.Id
                        span(index) += WingRegion.Length
                        WingRnk(index) = SurfaceRank
                        SurfaceRank += 2
                        index += 1
                    End If
                Next
            Else
                Design.FuselageID = Surface.Id
            End If
        Next
        If span(1) > span(0) Then
            Design.ElevatorID = id(0)
            Design.ElevatorRank = WingRnk(0)
            Design.MainWingID = id(1)
            Design.WingRank = WingRnk(1)
        Else
            Design.ElevatorID = id(1)
            Design.ElevatorRank = WingRnk(1)
            Design.MainWingID = id(0)
            Design.WingRank = WingRnk(0)
        End If
        Design.FuselageRank = SurfaceRank '''Fuselage is ALWAYS ranked LAST in solver!!!
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        NumericUpDown1.Enabled = CheckBox1.Checked
        NumericUpDown2.Enabled = CheckBox1.Checked
        If CheckBox1.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        NumericUpDown3.Enabled = CheckBox2.Checked
        NumericUpDown4.Enabled = CheckBox2.Checked
        If CheckBox2.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox3.CheckedChanged
        NumericUpDown5.Enabled = CheckBox3.Checked
        NumericUpDown6.Enabled = CheckBox3.Checked
        If CheckBox3.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox4.CheckedChanged
        NumericUpDown7.Enabled = CheckBox4.Checked
        NumericUpDown8.Enabled = CheckBox4.Checked
        If CheckBox4.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox5_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox5.CheckedChanged
        NumericUpDown9.Enabled = CheckBox5.Checked
        NumericUpDown10.Enabled = CheckBox5.Checked
        If CheckBox5.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox6_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox6.CheckedChanged
        NumericUpDown11.Enabled = CheckBox6.Checked
        NumericUpDown12.Enabled = CheckBox6.Checked
        If CheckBox6.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox7_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox7.CheckedChanged
        NumericUpDown13.Enabled = CheckBox7.Checked
        NumericUpDown14.Enabled = CheckBox7.Checked
        If CheckBox7.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox8_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox8.CheckedChanged
        NumericUpDown20.Enabled = CheckBox8.Checked
        NumericUpDown21.Enabled = CheckBox8.Checked
        If CheckBox8.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    Private Sub CheckBox9_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox9.CheckedChanged
        NumericUpDown22.Enabled = CheckBox9.Checked
        NumericUpDown23.Enabled = CheckBox9.Checked
        If CheckBox9.Checked Then
            NumericUpDown19.Value += 1
        Else
            NumericUpDown19.Value -= 1
        End If
    End Sub

    'This Sub set the constrains For the ACC (it Is To be used only In constrained optimization algorithm)
    Private Sub CheckBox10_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox10.CheckedChanged

        '    CheckBox3.Checked = False
        CheckBox4.Checked = False
        '    CheckBox3.Enabled = Not CheckBox10.Checked
        CheckBox4.Enabled = Not CheckBox10.Checked

        '    'fuselage length
        '    CheckBox9.Checked = CheckBox10.Checked
        '    CheckBox9.Enabled = Not CheckBox10.Checked
        '    NumericUpDown22.Minimum = 0.3
        '    NumericUpDown23.Maximum = 2

        '    'Tipchord
        '    CheckBox2.Checked = CheckBox10.Checked
        '    CheckBox2.Enabled = Not CheckBox10.Checked
        '    NumericUpDown3.Minimum = 0
        '    NumericUpDown4.Maximum = 0.1

        '    'Rootchord
        '    CheckBox1.Checked = CheckBox10.Checked
        '    CheckBox1.Enabled = Not CheckBox10.Checked
        '    NumericUpDown1.Enabled = False
        '    NumericUpDown1.Value = 0
        '    NumericUpDown2.Enabled = False
        '    NumericUpDown2.Value = 0

        '    'wing long pos
        '    CheckBox7.Checked = CheckBox10.Checked
        '    CheckBox7.Enabled = Not CheckBox10.Checked
        '    NumericUpDown13.Enabled = False
        '    NumericUpDown13.Value = 0
        '    NumericUpDown14.Enabled = False
        '    NumericUpDown14.Value = 0

    End Sub


End Class
