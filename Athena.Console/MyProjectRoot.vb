Imports OpenVOGEL.DesignTools.VisualModel.Models
Imports OpenVOGEL.AeroTools.CalculationModel.Solver
Imports OpenVOGEL.AeroTools.CalculationModel.Settings
Imports System.Xml

Public Module MyProjectRoot

    Public Property Name As String = "New aircraft"

    Public Property FilePath As String = ""

    Public Property SimulationSettings As New SimulationSettings

    Public Property Model As DesignModel = New DesignModel

    Public Property Results As New ResultModel


    Public Sub Initialize()

        SimulationSettings.InitializaParameters()
        Results.Clear()

    End Sub

    Public Sub RestartProject()

        Name = "New aircraft"
        Model.Objects.Clear()
        SimulationSettings.InitializaParameters()
        Results.Clear()

    End Sub

    Public ReadOnly Property ExistsOnDatabase As Boolean
        Get
            Return System.IO.File.Exists(FilePath)
        End Get
    End Property

#Region " Basic calculation launcher "

    Public Sub StartCalculation(ByVal Type As CalculationType, ByRef CalculationCore As Solver)

        SimulationSettings.AnalysisType = Type

        Try

            Dim BuildStructure As Boolean = Type = CalculationType.Aeroelastic
            CalculationCore.GenerateFromExistingModel(Model, SimulationSettings, BuildStructure)

            AddHandler CalculationCore.CalculationDone, AddressOf CalculationFinished

            Dim StartingTime As Date = Now

            Select Case Type

                Case CalculationType.SteadyState

                    CalculationCore.SteadyStateTransit(FilePath)

                Case CalculationType.FreeFlight

                    CalculationCore.FreeFlight(FilePath)

                Case CalculationType.Aeroelastic
                    CalculationCore.AeroelasticTransit(FilePath)

            End Select

        Catch ex As Exception
            RaiseEvent CalculationDone()
            Return
        End Try

    End Sub

    ''' <summary>
    ''' Handles the calculation finished of the solver
    ''' </summary>
    Private Sub CalculationFinished()

        RaiseEvent CalculationDone()

    End Sub

    ''' <summary>
    ''' Occurs when the calculation finishes.
    ''' </summary>
    ''' <remarks></remarks>
    Public Event CalculationDone()

#End Region

#Region " Input/Output "

    ''' <summary>
    ''' Reads a full project from XML file
    ''' </summary>
    Public Sub ReadFromXML()

        If Not ExistsOnDatabase Then Exit Sub

        Dim Reader As XmlReader = XmlReader.Create(FilePath)

        If Reader.ReadToDescendant("Project") Then

            ReadFromXML(Reader)

        End If

        Reader.Close()

    End Sub

    ''' <summary>
    ''' Reads a full project from XML subtree
    ''' </summary>
    Public Sub ReadFromXML(Reader As XmlReader)

        Name = Reader.GetAttribute("Name")

        While Reader.Read

            Select Case Reader.Name

                Case "Model"
                    Model.ReadFromXML(Reader.ReadSubtree)

                Case "Simulacion"
                    SimulationSettings.ReadFromXML(Reader.ReadSubtree)

            End Select

        End While

    End Sub

    ''' <summary>
    ''' Writes the full project to an XML file
    ''' </summary>
    Public Sub WriteToXML()

        Dim Writer As XmlWriter = XmlWriter.Create(FilePath)

        Writer.WriteStartElement("OpenVOGEL")

        Writer.WriteStartElement("Project")
        WriteToXML(Writer)
        Writer.WriteEndElement()

        Writer.WriteEndElement()

        Writer.Close()

    End Sub

    ''' <summary>
    ''' Writes the full project to an XML subtree
    ''' </summary>
    Public Sub WriteToXML(Writer As XmlWriter)

        Writer.WriteAttributeString("Name", Name)

        Writer.WriteStartElement("Model")
        Model.WriteToXML(Writer)
        Writer.WriteEndElement()

        Writer.WriteStartElement("Simulacion")
        SimulationSettings.SaveToXML(Writer)
        Writer.WriteEndElement()

    End Sub

    ''' <summary>
    ''' Loads the results from an XML results file
    ''' </summary>
    ''' <param name="ReferenceFilePath"></param>
    Public Sub ReadResults(ByVal ReferenceFilePath As String)

        Results.Clear()
        Results.LoadFromDirectory(IO.Path.GetDirectoryName(ReferenceFilePath))

    End Sub

    Public Enum ExportTypes As Integer
        ExportStl
        ExportCalculix
    End Enum

    ''' <summary>
    ''' Exports the model geometry in the given format.
    ''' </summary>
    ''' <param name="Type"></param>
    ''' <param name="OneFile"></param>
    Public Sub ExportDesignModel(Type As ExportTypes, OneFile As Boolean)

        Dim FileName As String = Strings.Left(FilePath, FilePath.Length - 4)

        Select Case Type

            Case ExportTypes.ExportStl

                If OneFile Then

                    ' Write all solids in a single file

                    FileName = FileName & ".stl"

                    Dim FileId As Integer = FreeFile()
                    FileOpen(FileId, FileName, OpenMode.Output)
                    PrintLine(FileId, "solid Model")
                    FileClose(FileId)

                    For Each Surface In Model.Objects
                        Surface.ExportStlFile(FileName, True)
                    Next

                    FileId = FreeFile()
                    FileOpen(FileId, FileName, OpenMode.Append)
                    PrintLine(FileId, "endsolid Model")
                    FileClose(FileId)

                Else

                    ' Write each model surface in a different file

                    Dim I As Integer = 0

                    For Each Surface In Model.Objects

                        Dim Name As String = Surface.Name

                        If Name = "" Then
                            Name = "Surface_" & I
                        End If

                        Surface.ExportStlFile(FileName & "_" & Name & ".stl", False)

                    Next

                End If

            Case ExportTypes.ExportCalculix

                If OneFile Then

                    ' Write all solids in a single file

                    FileName = FileName & ".dat"

                    For Each Surface In Model.Objects
                        Surface.ExportNativeFile(FileName, True)
                    Next

                Else

                    ' Write each model surface in a different file

                    Dim I As Integer = 0

                    For Each Surface In Model.Objects

                        Dim Name As String = Surface.Name

                        If Name = "" Then
                            Name = "Surface_" & I
                        End If

                        Surface.ExportNativeFile(FileName & "_" & Name & ".dat", False)

                    Next

                End If

        End Select

    End Sub

#End Region

End Module

