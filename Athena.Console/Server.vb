Imports System.Net
Imports System.Net.Sockets
Imports System.Text
Imports OpenVOGEL.AeroTools.CalculationModel
Imports OpenVOGEL.DesignTools

Public Module Server

    ''' <summary>
    ''' Sends back a message to the client
    ''' </summary>
    ''' <param name="Message"></param>
    ''' <param name="Address"></param>
    Private Sub Squeak(Squeaker As UdpClient, Message As String)
        Dim Bytes As Byte() = Text.Encoding.ASCII.GetBytes(Message)
        Squeaker.Send(Bytes, Bytes.Count)
    End Sub

    Public Sub RunServer()

        Dim Receiver As New UdpClient
        Receiver.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, True)
        Receiver.Client.Bind(New IPEndPoint(IPAddress.Any, 11000))

        Dim Quit As Boolean = False

        While Not Quit

            Dim ClientAddress As New IPEndPoint(IPAddress.Any, 11001)
            Dim Message As String = Encoding.ASCII.GetString(Receiver.Receive(ClientAddress))
            System.Console.WriteLine(ClientAddress.ToString & ":" & Message)

            Dim Commands As String() = Message.Split({";"c}, StringSplitOptions.RemoveEmptyEntries)

            If Commands.Count > 0 Then

                Select Case Commands(0)

                    ' Stop the service
                    Case "quit"

                        Quit = True

                    ' Run steady analysis
                    Case "steady"

                        If Commands.Count > 1 Then
                            System.Console.WriteLine("Steady state requested")
                            DataStore.FilePath = Commands(1)
                            RunCalculation(Settings.CalculationType.SteadyState)
                        End If

                    ' Run free flight simulation
                    Case "free_flight"

                        If Commands.Count > 1 Then
                            System.Console.WriteLine("Free flight simulation requested")
                            DataStore.FilePath = Commands(1)
                            RunCalculation(Settings.CalculationType.FreeFlight)
                        End If

                    ' Run free flight simulation
                    Case "aeroelastic"

                        If Commands.Count > 1 Then
                            System.Console.WriteLine("Aeroelastic simulation requested")
                            DataStore.FilePath = Commands(1)
                            RunCalculation(Settings.CalculationType.Aeroelastic)
                        End If

                End Select

            End If

        End While

        Receiver.Close()

    End Sub

    Private Sub RunCalculation(Type As Settings.CalculationType)

        Dim Squeaker As New UdpClient
        Dim CalculationCore As New Solver.Solver

        Try

            Squeaker.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, True)
            Squeaker.Connect("localhost", 11001)

            ' Perform calculation

            MyProjectRoot.ReadFromXML()
            DataStore.StartCalculation(Type, CalculationCore)

        Finally

            Squeak(Squeaker, "done;" & CalculationCore.BaseDirectoryPath)
            Squeaker.Close()

        End Try

    End Sub

    Private Sub OutputConsoleMessage(Message As String)
        System.Console.WriteLine(Message)
    End Sub

    Private Sub OutputConsoleProgress(Title As String, Value As Integer)
        System.Console.WriteLine(Title)
    End Sub

End Module

