VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Space Invaders by Walter A. Narvasa"
   ClientHeight    =   3780
   ClientLeft      =   1590
   ClientTop       =   1725
   ClientWidth     =   5745
   ClipControls    =   0   'False
   Icon            =   "shoot.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   Picture         =   "shoot.frx":0442
   ScaleHeight     =   3780
   ScaleWidth      =   5745
   ShowInTaskbar   =   0   'False
   Begin VB.Timer Timer5 
      Left            =   6600
      Top             =   135
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      FillColor       =   &H00FF0000&
      FillStyle       =   0  'Solid
      Height          =   330
      Index           =   9
      Left            =   8115
      Picture         =   "shoot.frx":2A195
      ScaleHeight     =   330
      ScaleWidth      =   360
      TabIndex        =   6
      Top             =   930
      Width           =   360
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      FillColor       =   &H00FF0000&
      FillStyle       =   0  'Solid
      Height          =   330
      Index           =   10
      Left            =   8085
      Picture         =   "shoot.frx":2A31F
      ScaleHeight     =   330
      ScaleWidth      =   360
      TabIndex        =   5
      Top             =   1245
      Width           =   360
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      FillColor       =   &H00FF0000&
      FillStyle       =   0  'Solid
      Height          =   330
      Index           =   20
      Left            =   8115
      Picture         =   "shoot.frx":2A4A9
      ScaleHeight     =   330
      ScaleWidth      =   360
      TabIndex        =   4
      Top             =   1590
      Width           =   360
   End
   Begin VB.PictureBox Picture1 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      FillColor       =   &H00FF0000&
      FillStyle       =   0  'Solid
      Height          =   330
      Index           =   39
      Left            =   8100
      Picture         =   "shoot.frx":2A633
      ScaleHeight     =   330
      ScaleWidth      =   360
      TabIndex        =   3
      Top             =   1905
      Width           =   360
   End
   Begin VB.PictureBox Picture3 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   4530
      Left            =   540
      Picture         =   "shoot.frx":2A7BD
      ScaleHeight     =   4500
      ScaleWidth      =   6960
      TabIndex        =   2
      Top             =   3345
      Width           =   6990
   End
   Begin VB.Timer Timer4 
      Left            =   3060
      Top             =   15
   End
   Begin VB.Timer Timer3 
      Left            =   2535
      Top             =   15
   End
   Begin VB.Timer Timer2 
      Interval        =   1
      Left            =   2010
      Top             =   15
   End
   Begin VB.Timer Timer1 
      Left            =   1470
      Top             =   15
   End
   Begin VB.Image Image7 
      Height          =   195
      Index           =   3
      Left            =   4395
      Picture         =   "shoot.frx":2CF96
      Top             =   2715
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image7 
      Height          =   195
      Index           =   2
      Left            =   3090
      Picture         =   "shoot.frx":2D5E0
      Top             =   2715
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image7 
      Height          =   195
      Index           =   1
      Left            =   1830
      Picture         =   "shoot.frx":2DC2A
      Top             =   2715
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image7 
      Height          =   195
      Index           =   0
      Left            =   630
      Picture         =   "shoot.frx":2E274
      Top             =   2715
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   5
      Left            =   3690
      Picture         =   "shoot.frx":2E8BE
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   4
      Left            =   3120
      Picture         =   "shoot.frx":2EF08
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   3
      Left            =   2550
      Picture         =   "shoot.frx":2F552
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   2
      Left            =   1995
      Picture         =   "shoot.frx":2FB9C
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   1
      Left            =   1440
      Picture         =   "shoot.frx":301E6
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image6 
      Appearance      =   0  'Flat
      Height          =   195
      Index           =   0
      Left            =   885
      Picture         =   "shoot.frx":30830
      Top             =   2145
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   39
      Left            =   915
      Picture         =   "shoot.frx":30E7A
      Top             =   1110
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   38
      Left            =   3960
      Picture         =   "shoot.frx":30F54
      Top             =   945
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   37
      Left            =   240
      Picture         =   "shoot.frx":3102E
      Top             =   1875
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   36
      Left            =   330
      Picture         =   "shoot.frx":31108
      Top             =   2325
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   35
      Left            =   3540
      Picture         =   "shoot.frx":311E2
      Top             =   1065
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   34
      Left            =   495
      Picture         =   "shoot.frx":312BC
      Top             =   1035
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   33
      Left            =   270
      Picture         =   "shoot.frx":31396
      Top             =   1230
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   32
      Left            =   165
      Picture         =   "shoot.frx":31470
      Top             =   1380
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   31
      Left            =   3885
      Picture         =   "shoot.frx":3154A
      Top             =   1725
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   30
      Left            =   720
      Picture         =   "shoot.frx":31624
      Top             =   1740
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   29
      Left            =   660
      Picture         =   "shoot.frx":316FE
      Top             =   990
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   28
      Left            =   3390
      Picture         =   "shoot.frx":317D8
      Top             =   1170
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   27
      Left            =   600
      Picture         =   "shoot.frx":318B2
      Top             =   1890
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   26
      Left            =   780
      Picture         =   "shoot.frx":3198C
      Top             =   1590
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   25
      Left            =   165
      Picture         =   "shoot.frx":31A66
      Top             =   1590
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   24
      Left            =   375
      Picture         =   "shoot.frx":31B40
      Top             =   1140
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   23
      Left            =   3600
      Picture         =   "shoot.frx":31C1A
      Top             =   1545
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   22
      Left            =   3690
      Picture         =   "shoot.frx":31CF4
      Top             =   1380
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   21
      Left            =   3240
      Picture         =   "shoot.frx":31DCE
      Top             =   1455
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   20
      Left            =   3255
      Picture         =   "shoot.frx":31EA8
      Top             =   1755
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   19
      Left            =   3825
      Picture         =   "shoot.frx":31F82
      Top             =   945
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   18
      Left            =   3630
      Picture         =   "shoot.frx":3205C
      Top             =   1020
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   17
      Left            =   555
      Picture         =   "shoot.frx":32136
      Top             =   2040
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   16
      Left            =   450
      Picture         =   "shoot.frx":32210
      Top             =   2175
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   15
      Left            =   3900
      Picture         =   "shoot.frx":322EA
      Top             =   1245
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   14
      Left            =   3960
      Picture         =   "shoot.frx":323C4
      Top             =   1065
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   13
      Left            =   3375
      Picture         =   "shoot.frx":3249E
      Top             =   1875
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   12
      Left            =   3570
      Picture         =   "shoot.frx":32578
      Top             =   1920
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   11
      Left            =   3450
      Picture         =   "shoot.frx":32652
      Top             =   1155
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   10
      Left            =   3195
      Picture         =   "shoot.frx":3272C
      Top             =   1575
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   9
      Left            =   420
      Picture         =   "shoot.frx":32806
      Top             =   1890
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   8
      Left            =   795
      Picture         =   "shoot.frx":328E0
      Top             =   960
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   7
      Left            =   3300
      Picture         =   "shoot.frx":329BA
      Top             =   1305
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   6
      Left            =   3780
      Picture         =   "shoot.frx":32A94
      Top             =   1875
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   5
      Left            =   585
      Picture         =   "shoot.frx":32B6E
      Top             =   1770
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   4
      Left            =   195
      Picture         =   "shoot.frx":32C48
      Top             =   1785
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   3
      Left            =   735
      Picture         =   "shoot.frx":32D22
      Top             =   1305
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   2
      Left            =   960
      Picture         =   "shoot.frx":32DFC
      Top             =   1020
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   1
      Left            =   585
      Picture         =   "shoot.frx":32ED6
      Top             =   1005
      Width           =   210
   End
   Begin VB.Image Image5 
      Appearance      =   0  'Flat
      Height          =   165
      Index           =   0
      Left            =   825
      Picture         =   "shoot.frx":32FB0
      Top             =   1200
      Width           =   210
   End
   Begin VB.Label Label4 
      BackStyle       =   0  'Transparent
      Caption         =   "00000"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   255
      Left            =   4890
      TabIndex        =   1
      Top             =   15
      Width           =   585
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   "Score"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   255
      Left            =   4290
      TabIndex        =   0
      Top             =   15
      Width           =   600
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   7
      Left            =   5610
      Picture         =   "shoot.frx":3308A
      Top             =   465
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   6
      Left            =   5190
      Picture         =   "shoot.frx":331A8
      Top             =   1830
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   5
      Left            =   5190
      Picture         =   "shoot.frx":332C6
      Top             =   1620
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   4
      Left            =   5190
      Picture         =   "shoot.frx":333E4
      Top             =   1410
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   3
      Left            =   5610
      Picture         =   "shoot.frx":33502
      Top             =   195
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   2
      Left            =   5190
      Picture         =   "shoot.frx":33620
      Top             =   1200
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   1
      Left            =   5190
      Picture         =   "shoot.frx":3373E
      Top             =   1005
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image4 
      Height          =   195
      Index           =   0
      Left            =   5190
      Picture         =   "shoot.frx":3385C
      Top             =   795
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image3 
      Height          =   195
      Index           =   2
      Left            =   720
      Picture         =   "shoot.frx":3397A
      Top             =   90
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image3 
      Height          =   195
      Index           =   1
      Left            =   435
      Picture         =   "shoot.frx":33A98
      Top             =   90
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Image Image3 
      Height          =   195
      Index           =   0
      Left            =   135
      Picture         =   "shoot.frx":33BB6
      Top             =   90
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      Index           =   3
      X1              =   2040
      X2              =   2040
      Y1              =   510
      Y2              =   705
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      Index           =   2
      X1              =   2280
      X2              =   2280
      Y1              =   510
      Y2              =   705
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFF00&
      Index           =   1
      X1              =   2385
      X2              =   2385
      Y1              =   510
      Y2              =   705
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFF00&
      BorderStyle     =   2  'Dash
      Index           =   0
      X1              =   2160
      X2              =   2160
      Y1              =   510
      Y2              =   705
   End
   Begin VB.Image Image1 
      Height          =   165
      Index           =   1
      Left            =   3675
      Picture         =   "shoot.frx":33CD4
      Top             =   1440
      Visible         =   0   'False
      Width           =   210
   End
   Begin VB.Line Line1 
      BorderColor     =   &H0000FF00&
      X1              =   5325
      X2              =   5325
      Y1              =   465
      Y2              =   690
   End
   Begin VB.Image Image1 
      Height          =   165
      Index           =   0
      Left            =   3825
      Picture         =   "shoot.frx":33DAE
      Top             =   1305
      Visible         =   0   'False
      Width           =   210
   End
   Begin VB.Image Image2 
      Height          =   195
      Left            =   5190
      Picture         =   "shoot.frx":33E88
      Top             =   585
      Visible         =   0   'False
      Width           =   270
   End
   Begin VB.Menu start_game 
      Caption         =   "Play"
   End
   Begin VB.Menu help 
      Caption         =   "Help"
   End
   Begin VB.Menu About 
      Caption         =   "About"
   End
   Begin VB.Menu leave_game 
      Caption         =   "Exit"
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Space Invaders written in Visual Basic v4.0
' by Walter A. Narvasa

' Define global variables for use in the game
Const max_men = 40
Const num_alien_missiles = 4

' short cut to save checking and moving totally dead rows.
Dim row_dead(10) As Boolean

' Aliens need to know where the ground is as I didn't
' bother drawing one. This is the top of the ship to see if
' they will hit it on their next traverse of the screen
Dim planet_level As Integer
Dim left_edge, right_edge As Integer

Dim score As Integer ' self expl.

' level is the level at which the player is at.
' if level equals one then the player has just begun
' in which case, give three lives etc...
' otherwise, set the aliens up and carry on.
Dim level As Integer
Dim game_in_progress As Boolean


' flag if alien missile is flying.
Dim alien_missile(num_alien_missiles) As Boolean
Dim alien_miss_speed(num_alien_missiles), alien_miss_length(num_alien_missiles) As Integer

' max_men is set as the number of aliens.  To change
' use EDIT, replace CONSTANT max_men with new number.
' xstep and ystep set as arrays in case of need to
' fly alilens down in a Galaxians SWOOOOOP manoever!
Dim xstep(max_men), ystep(max_men) As Integer

'Shield information ...
Dim num_shields As Integer
Dim shield_strength(3) As Integer



Dim image2_bottom As Integer ' stores the base line for the game (here lies planet earth).


' Max x and Max y store the size of the current screen
Dim maxx, maxy As Integer

' If alive is false, dont move, display or check impact
' of bullet with aliens - may speed the game up!!!
Dim alive(max_men) As Boolean
Dim lives_left As Integer

' Num Men should be set to 30 or however many aliens are needed
Dim num_men As Integer

' define the shape of the alien attack.
Dim men_per_row As Integer
Dim num_rows, left_once As Integer

' the picture to be loaded into the alien.
Dim alien_shape As Integer

' Useful little variables to only check the extreme alliens
' which are alive for beyond edge of screen checks.
' either of these aliens crosses the edge, ALL turn round
Dim right_most_man As Integer
Dim left_most_man As Integer

' Used to determine whether the missile has hit the critter.
Dim alien_width, alien_height As Integer
Dim valien_spacing, halien_spacing As Integer
Dim valien_speed, halien_speed As Integer

' If player wants to move, need to do it smoothly.
' this is handled over a number of timer intervals
' the distance of each move defined as num_player_moves.
' distance each interval held in player_speed.
Dim moving_left, moving_right, firing As Boolean
Dim num_player_moves, player_speed As Integer
Dim player_dying As Boolean
Dim player_dying_image As Integer

' used for checking missile impact (aliens of course)
Dim player_width As Integer

' defines the size of the bullet and the distance it
' moves each timer event.
Dim bullet_length, bullet_speed As Integer

' T.B.A.
Dim dying As Boolean
Dim dying_alien As Integer
Dim all_dead As Boolean

' Number of moves until player is squashed by aliens at end
Dim num_moves_to_die As Integer
Dim death_count As Integer

Dim row_to_move As Integer

' Allow time for sound effects to be played before next shot.
' may also delay shooting whilst death in progress is set.
Dim firing_delay, time_to_next_shot As Integer

' As the aliens are shot, the speed at which they move should be
' increased.  the alien_start_speed is the initial time event
' the speed up factor is the drop in this delay per kill.
Dim speed_up_factor, alien_start_speed As Integer


Dim player_move_interval, player_die_interval As Integer

' define sound settings - Works with windows sound system.
' If placed in module outside the form, need to remove PRIVATE definition
Private Declare Function sndPlaySound Lib "WINMM.DLL" Alias _
      "sndPlaySoundA" (ByVal lpszSoundName As String, ByVal uFlags As _
      Long) As Long

Const SND_ASYNC = &H1



Private Sub About_Click()
    Dim msg
    msg = MsgBox("This is programmed by Walter A. Narvasa")
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)


'OK, the keys have been pressed. May be better as mouse movements
'but for now it is all hands on keys.
If Not player_dying Then
 Select Case KeyAscii
 Case 122, 44    '  Z or <
     moving_left = True
     moving_right = False
     num_player_moves = 4
 Case 120, 46    '  x or >
     moving_right = True
     moving_left = False
     num_player_moves = 4
 Case 32                    ' good old space bar takes a bashing
     If Not (firing) Then   ' only allow to fire if not already firing.
      If time_to_next_shot < 0 Then
       firing = True        ' too tricky at the mo to allow multiple shots
       Line1.X1 = Image2.Left + (Image2.Width / 2)  'Missile starts at fire point of Bitmap
       Line1.X2 = Line1.X1
       Line1.Y1 = Image2.Top
       Line1.Y2 = Line1.Y1 - bullet_length
       Line1.Visible = True
       sndPlaySound App.Path + "\plr_fire.wav", SND_ASYNC
      End If
     End If
 End Select
End If ' not player_dying...



End Sub

Private Sub game_over()

sndPlaySound App.Path + "\gameover.wav", 0   ' oh no - byeee


Image2.Visible = False

Call Form_Load          ' lets pretend it has just been switched on

End Sub


Private Sub Form_Load()

game_in_progress = False



Label3.Visible = False
Label4.Visible = False



Picture3.Visible = False  ' hide the help
Picture3.Left = -550
Picture3.Top = -180


level = 1 ' very important - half an hour to suss that one out.

' Allows modification of the bitmaps without having to
' load them in all the time.  Code handles it quite well.
Image1(0).Picture = LoadPicture(App.Path + "\alien1.bmp")
Image1(1).Picture = LoadPicture(App.Path + "\alien2.bmp")

firing_delay = 2
time_to_next_shot = -1

'define alien dimensions for intersection check
alien_width = Image5(0).Width + 1
alien_height = Image5(0).Height + 1

' move the player ship to the bottom left of the screen
Image2.Left = 100
Image2.Top = maxy - (Image2.Height * 4)

'hide the bullet then define the length and speed
Line1.Visible = False
Line2(0).Visible = False
Line2(1).Visible = False
Line2(2).Visible = False
Line2(3).Visible = False

bullet_length = 200
bullet_speed = 150

'num_men is the number of aliens at the start
num_men = 40

'switch on keyboard logging events
KeyPreview = True

' hide the aliens
left_once = 0
looper = 0
While looper < num_men
   Image5(looper).Visible = 0
   Image5(looper).Picture = Image1(alien_shape).Picture
   looper = looper + 1
Wend


' work out the size of the screen
' If set to "sizeable" during further developments
' this will handle the positions of all components
maxx = Form1.Width - Image5(0).Width - 100
maxy = Form1.Height - Image5(0).Height - 100

left_edge = 200
right_edge = maxx - 200

'Switch off the timer for alien movements
Timer1.Interval = 0
Timer1.Enabled = False

End Sub

Private Sub Form_Resize()

' move the button.
' work out the size of the screen - still sizeable!!
maxx = Form1.Width - Image5(0).Width - 100
maxy = Form1.Height - Image5(0).Height - 100

'Command1.Left = maxx / 2 - (Command1.Width / 2)
'Command1.Top = maxy / 2 - (Command1.Height / 2)

End Sub





Private Sub help_Click()

If Not game_in_progress Then

  ' Wow what a cop out.  It is now getting late.
  Picture3.Visible = True
End If

End Sub



Private Sub leave_game_Click()

left_once = left_once + 1


Timer1.Enabled = False
Timer2.Enabled = False
Timer3.Enabled = False
Timer4.Enabled = False
Timer5.Enabled = True
Timer5.Interval = 30000

sndPlaySound App.Path + "\doorslam", 0

If left_once = 2 Then End

End Sub






Private Sub Picture3_Click()

Picture3.Visible = False

End Sub

Private Sub start_game_Click()

'
'  Start Game - If level = 1 then initialise the
'               local variables.
'
'               otherwise, rack em up for the next
'               level.
'
'  Initialisation stuff should really be in INIT routine
'

' local declarations
' direction is probably not used anymore.
Dim direction As Integer
Dim row_loop As Integer


game_in_progress = True

If level = 1 Then ' this is the first time this has been called.

  Label3.Visible = True
  Label4.Visible = True
  
 
  Picture3.Visible = False
  
  player_dying_image = 0   ' Not dead yet


  lives_left = 3              ' Traditional 3 lives trick.
  Image3(0).Visible = True    ' set three ships at the top of the form
  Image3(1).Visible = True    ' to visible.  Turn each off when dead
  Image3(2).Visible = True

  alien_miss_length(0) = 200   ' Vary these to alter the appearence
  alien_miss_length(1) = 200   ' of the missiles.
  alien_miss_length(2) = 200   '
  alien_miss_length(3) = 200   ' Play with the lines on the form (dotted etc.)


  score = 0              ' reset the score
  Label4.Caption = score ' and display it
  Label3.Visible = True  ' Turn on the score
  Label4.Visible = True  ' boxes

  halien_spacing = Image1(0).Width * 1.7    'Set horizontal space between green things to one green thing
  valien_spacing = Image1(0).Height * 1.5   'Set vertical space between green things to one green thing
 
  valien_speed = Int(valien_spacing * 0.75) 'Proportional speed
  halien_speed = Int(halien_spacing * 0.5)  'Proportional descent too!

  maxx = Form1.Width - Image5(0).Width - 160    ' Used for all sorts of things related to the size of the screen
  maxy = Form1.Height - Image5(0).Height - 100  ' as above

  Screen.MousePointer = 2    ' Change mouse pointer to crosshair. no reason!

End If ' level = 1 and the initialisation has to be done.

num_shields = 3

'restore (or create) the shields
row_loop = 0
While row_loop <= num_shields
   Image7(row_loop).Picture = Image6(0).Picture
   Image7(row_loop).Top = 2730
   shield_strength(row_loop) = 10
   Image7(row_loop).Visible = True
   row_loop = row_loop + 1
Wend


' Now set the aliens up for the next assault.

row_loop = 0
While row_loop < 10
  row_dead(row_loop) = False
  row_loop = row_loop + 1
Wend

' Kill all alien missiles - Vital if starting second level!

row_loop = 0
While row_loop < num_alien_missiles
  alien_missile(row_loop) = False
  alien_miss_speed(row_loop) = (row_loop * 30) + (alien_miss_length(row_loop) / 5)
  Line2(row_loop).Visible = False
  Line2(row_loop).X1 = 0
  Line2(row_loop).X2 = 0
  Line2(row_loop).Y1 = 0
  Line2(row_loop).Y2 = 0
  row_loop = row_loop + 1
Wend


men_per_row = 7  ' Play around with this!
num_rows = Int(num_men / men_per_row)   ' calculate based on max num aliens allowed.
row_to_move = num_rows - 1              ' the first row of aliens to shift

num_men = men_per_row * num_rows        ' Ignore those which failed to make it into the grid.

alien_start_speed = 700 - (level * 100)
speed_up_factor = 700 / (num_men + 1)

alien_shape = 0   ' start with the default alien

all_dead = True   ' the aim of the game.

player_move_interval = 15 ' Player timer interval - 15-25 works fine.
player_die_interval = 150 ' slow down for the big explosion.

Image2.Visible = True     'Lets see the players ship

Timer1.Interval = 0       ' turn off the timer
Timer1.Enabled = False    '

' loop over aliens and position them on the screen
' aliens still invisible at this point.
row_loop = 0
While row_loop < num_rows
  looper = 0
  While looper < men_per_row
    Image5(looper + (row_loop * men_per_row)).Top = 500 + (level * 200) + (row_loop * valien_spacing)
    Image5(looper + (row_loop * men_per_row)).Left = 300 + (looper * halien_spacing)
    looper = looper + 1
  Wend
  row_loop = row_loop + 1
Wend

' Put the player on the screen at last - There y go.
Image2.Left = 100
Image2.Top = maxy - (Image2.Height * 4)
image2_bottom = Image2.Top + Image2.Height

' Now define where the ground is (for alien Landing)
planet_level = Image2.Top - player_height ' defines when the aliens have landed.

' Set the speed of the player movement to 60.
player_speed = 60


' Load the aliens with the healthy alien image.
' allows for killing aliens at a later date.
' also, show them on the screen, set their motion
' and give them the secret of LIFE ITSELF - ROCKY UGH!!

looper = 0
While looper < num_men
    Image5(looper).Picture = Image1(alien_shape).Picture
    Image5(looper).Visible = True
    alive(looper) = True
    xstep(looper) = halien_speed
    ystep(looper) = 0
    looper = looper + 1
Wend

death_count = -1

' set the alien movement timer to start speed - may be too quick on decent PCs.
Timer1.Interval = alien_start_speed
Timer1.Enabled = True

' switch on the timer for the player movement
Timer2.Interval = player_move_interval
Timer2.Enabled = True

' switch on the timer for the bullet firing.
Timer3.Interval = 20
Timer3.Enabled = True

' switch on the timer for the alien missiles
Timer4.Interval = 20
Timer4.Enabled = True


'scan the men to find the right and left most alien.
'this is the one to trigger the turn around.
looper = 0
biggest = -1         ' usual big/small
smallest = 10000     ' stuff

While looper < num_men
   If Image5(looper).Left > biggest Then
      biggest = Image5(looper).Left
      right_most_man = looper
   End If
   If Image5(looper).Left < smallest Then
      smallest = Image5(looper).Left
      left_most_man = looper
   End If
   looper = looper + 1
Wend

player_width = Image2.Width


End Sub

Private Sub timer1_Timer()


'declare local variables

Dim done_some As Integer          ' Are all the aliens dead??
Dim change_direction As Boolean   ' Are the aliens about to turn around?
Dim alien_offset, alien_of_interest As Integer
Dim alien, missile, alien_missile_check As Integer
Dim looking_for_alien As Boolean
Dim start_row As Integer
Dim checked As Boolean
Dim bin_loop As Integer

'main code

change_dir = False  ' it seems not
done_some = 0

start_row = row_to_move
While row_dead(row_to_move)
  row_to_move = row_to_move - 1
  If row_to_move = start_row Then row_dead(start_row) = False ' This avoids a nasty crash!
    If row_to_move = -1 Then
      row_to_move = num_rows - 1
      alien_shape = alien_shape + 1
    If alien_shape = 2 Then alien_shape = 0
  End If
Wend

checked = False

If Not (row_dead(row_to_move)) Then
 looper = 0
 ' Next bit moves the living aliens along a little.
 alien_offset = (row_to_move) * men_per_row
 While looper < men_per_row
   alien_of_interest = looper + alien_offset
   If alive(alien_of_interest) Then
     all_dead = False
     done_some = done_some + 1
     Image5(alien_of_interest).Picture = Image1(alien_shape).Picture
       
     'move the alien DOWN if needed - then stop it moving further
     If ystep(alien_of_interest) > 0 Then
       Image5(alien_of_interest).Top = Image5(alien_of_interest).Top + ystep(alien_of_interest)
       If Image5(alien_of_interest).Top > planet_level Then Call game_over
       ystep(alien_of_interest) = 0
        If Not checked Then
         checked = True
         If Image5(alien_of_interest).Top > 2715 Then
           bin_loop = 0
           While bin_loop <= num_shields
             Image7(bin_loop).Visible = False
             shield_strength(bin_loop) = -1
             bin_loop = bin_loop + 1
           Wend
         End If
        End If ' Not checked
     End If
     
     Image5(alien_of_interest).Left = Image5(alien_of_interest).Left + xstep(alien_of_interest)
     If ystep(alien_of_interest) > valien_spacing Then ystep(alien_of_interest) = 0
   End If
   
   ' Now check the extreme aliens to see if they have
   ' reached the edge of the screen.
   
   If alien_of_interest = left_most_man Then
     If Image5(alien_of_interest).Left < left_edge Then change_dir = True
   End If
   If alien_of_interest = right_most_man Then
     If Image5(alien_of_interest).Left > right_edge Then change_dir = True
   End If
   looper = looper + 1 ' Useful to put this outside the ALIVE loop
                       ' anywhere else and it crashes!!!

 Wend    ' End while
End If   ' if not row dead

If done_some = 0 Then row_dead(row_to_move) = True

' check if all rows have been moved - if so then change the alien shape
row_to_move = row_to_move - 1
If row_to_move < 0 Then
  row_to_move = num_rows - 1
  alien_shape = alien_shape + 1
  If alien_shape = 2 Then alien_shape = 0
  If all_dead Then
   ' odd place for the all aliens dead stuff, however here it is.
   ' the reason for it being here is that now we have checked all
   ' rows of aliens.  If none moved then all must be dead.
   level = level + 1
   Call start_game_Click
  End If
  all_dead = True ' But set false once one of them moves a bit.
End If

' All turn round
If change_dir Then
'   row_to_move = num_rows - 1
   looper = 0
   While looper < num_men
      xstep(looper) = -xstep(looper)
      ystep(looper) = valien_speed
      looper = looper + 1
   Wend
End If

' WELL YOU HAVE TO GIVE THE ALIENS A CHANCE!
' THE NUMBER OF MISSILES IS UP TO YOU, HOWEVER
' WITH THE MISSILES RAINING DOWN IT MAY SLOW UP THE
' GAME!    BEWARE!
' IF YOU GO ABOVE 4, THEN INCREASE THE SIZE OF THE LINE2 CONTROL ARRAY.

alien_missile_check = 0
While alien_missile_check < num_alien_missiles
 If Not (alien_missile(alien_missile_check)) Then
   missile = Int(Rnd * 7) - 1 'to fire or not to fire? One in eight is the answer
   If missile = 0 Then
     alien_missile(alien_missile_check) = True
     ' pick an alien to fire ...
     looking_for_alien = True
     
     num_tries = 0
     While looking_for_alien And num_tries < num_men
        num_tries = num_tries + 1
        alien = Int(Rnd * num_men)
        If alive(alien) Then looking_for_alien = False
     Wend
     
     If num_tries < num_men Then    ' ie not a dead firing dude...
       Line2(alien_missile_check).X1 = Image5(alien).Left + (alien_width / 2)
       Line2(alien_missile_check).X2 = Line2(alien_missile_check).X1
       Line2(alien_missile_check).Y1 = Image5(alien).Top + alien_height
       Line2(alien_missile_check).Y2 = Line2(alien_missile_check).Y1 + alien_miss_length(alien_missile_check)
       Line2(alien_missile_check).Visible = True
     End If
   End If
End If
alien_missile_check = alien_missile_check + 1
Wend



End Sub 'couldnt think of any more stuff to put in here.


Private Sub Timer2_Timer()

' timer 2 is going to handle the player movements
' and the shot being fired (for the moment).
' dim local variables

Dim smallest, biggest As Integer
Dim looper2 As Integer

' moving_left, moving_right, firing As Boolean
' num_player_moves As Integer

' update the firing delay
time_to_next_shot = time_to_next_shot - 1
If Not player_dying Then
 If moving_left Then
    num_player_moves = num_player_moves - 1
    If num_player_moves = 0 Then moving_left = False
    Image2.Left = Image2.Left - player_speed
    If Image2.Left < left_edge Then Image2.Left = left_edge + 2
 End If

 If moving_right Then
    num_player_moves = num_player_moves - 1
    If num_player_moves = 0 Then moving_right = False
    Image2.Left = Image2.Left + player_speed
    If Image2.Left > right_edge Then Image2.Left = right_edge - 2
 End If
 
 Else ' Player is dying - advance the movie ...
 
    If player_dying_image = 8 Then
       player_dying_image = 0
       player_dying = False
       If lives_left = 0 Then
         Timer2.Enabled = False  '
         Timer3.Enabled = False  ' STOP THE CLOCKS
         Timer4.Enabled = False  '
         Timer1.Enabled = False  '
         Call game_over   ' sounds etc then back to adverts
       End If
       Image2.Picture = Image3(0).Picture
       Timer2.Interval = player_move_interval
    Else
       Image2.Picture = Image4(player_dying_image).Picture
       player_dying_image = player_dying_image + 1
    End If
    
End If ' not player dying - ie ignore key input if player being killed.

' drop the death_count.  This prevents firing continuously.
death_count = death_count - 1
If death_count = 0 Then End
If death_count = -2 Then death_count = -1


End Sub


Private Sub Timer3_Timer()


'
' Timer 3 - Move the player missile
'
'



Dim temp_timer_loss As Integer
Dim temp_num_left As Integer
Dim shield_loop As Integer



If firing Then
   Line1.Y1 = Line1.Y1 - bullet_speed   ' move the bullet
   Line1.Y2 = Line1.Y2 - bullet_speed   ' upwards!!!!!
   If Line1.Y2 < 10 Then
      firing = False
      Line1.Visible = False
   End If
   
   If Line1.Y2 > Image7(0).Top Then ' we may have hit a shield
    If Line1.Y2 < Image7(0).Top + Image7(0).Height Then ' it is looking more likely
      
     'find if shield hit.
     shield_loop = 0
     While shield_loop <= num_shields
      If shield_strength(shield_loop) >= 0 Then
      
       'Check for shield impact with living shield
       If Line1.X1 > Image7(shield_loop).Left Then
        If Line1.X1 < Image7(shield_loop).Left + Image7(shield_loop).Width Then
          
         'boom !
         ' kill the missile - job done
         Line1.Visible = False
         Line1.Y1 = 1
         Line1.Y2 = 1
         firing = False

         shield_strength(shield_loop) = shield_strength(shield_loop) - 1
         If shield_strength(shield_loop) < 0 Then
          Image7(shield_loop).Visible = False
         Else
          Image7(shield_loop).Top = Image7(shield_loop).Top + 15
          If shield_strength(shield_loop) < 5 Then
           Image7(shield_loop).Picture = Image6(5 - shield_strength(shield_loop)).Picture
          End If
         End If
        End If ' < left+impact
       End If ' > left
      End If ' shield alive
     shield_loop = shield_loop + 1
     Wend
    End If
   End If
   

   
   
   
   
   
   ' check for impact with a little green critter
   ' IF statements set in least likely to be true order
   ' to save processing time.  Still not brilliant
   
   looper = 0
   While looper < num_men
      If alive(looper) Then
      
        If Image5(looper).Top + alien_height > Line1.Y1 Then
        If Image5(looper).Top < Line1.Y1 Then
        If Image5(looper).Left < Line1.X1 Then
        If (Image5(looper).Left + alien_width) > Line1.X1 Then
                      
           
           score = score + level * 10
           Label4.Caption = score
           
           time_to_next_shot = firing_delay
           firing = False
           Line1.Visible = False
                      
           xstep(looper) = 0
           ystep(looper) = 0
           
           temp_timer_loss = Timer1.Interval - speed_up_factor
           If temp_timer_loss < 1 Then temp_timer_loss = 1
           Timer1.Interval = temp_timer_loss

           Image5(looper).Visible = False
           alive(looper) = False

           sndPlaySound App.Path + "\move1.wav", 1

           'scan the men to find the right and left most alien.
           'this is the one to trigger the turn around.
           'y-dig ? - Only look at those which are alive
           'This is only needed if alien hit is left or right most.
           'Even then only look for the appropriate one (both if only one alien left
           
           temp_num_left = 0
           looper2 = 0
           While (looper2 < num_men) And (temp_num_left = 0)
             If alive(looper2) Then temp_num_left = 1
             looper2 = looper2 + 1
           Wend
           If temp_num_left = 0 Then
              level = level + 1
              Call start_game_Click ' disgusting exit I know but its getting late.
           End If
           
           If ((looper = left_most_man) Or (looper = right_most_man)) Then
             looper2 = 0
             
             biggest = 10000
             smallest = -1
 
             If looper = left_most_man Then smallest = 100000
             If looper = right_most_man Then biggest = -1
             
             'OK Lets get looking for the winning alien
             
             temp_num_left = 0
             While looper2 < num_men
               If alive(looper2) Then
               temp_num_left = temp_num_left + 1
               If Image5(looper2).Left > biggest Then
                biggest = Image5(looper2).Left
                right_most_man = looper2
               End If
               If Image5(looper2).Left < smallest Then
                smallest = Image5(looper2).Left
                left_most_man = looper2
               End If
               End If
               looper2 = looper2 + 1
             Wend
           End If
        End If    '
        End If    '  indent if you please
        End If    '
        End If    '
      End If
      looper = looper + 1
   Wend
End If

End Sub

Private Sub Timer4_Timer()

'
' Timer 4 - Move the alien missiles
'
' ps it is still raining and I have
'    now run out of Coffee
'

Dim alien_missile_check As Integer
Dim shield_loop As Integer

' code looks sticky due to lack of spaces and comments, however...
' its quite simple.  If missile has reached the bottom then check
' if it has landed in the middle of the players ship.
' if so, plaer dies, if not, missile disappears
alien_missile_check = 0
While alien_missile_check < num_alien_missiles
 If alien_missile(alien_missile_check) Then
   Line2(alien_missile_check).Y1 = Line2(alien_missile_check).Y1 + alien_miss_speed(alien_missile_check)   ' move the alien missile
   Line2(alien_missile_check).Y2 = Line2(alien_missile_check).Y2 + alien_miss_speed(alien_missile_check)   ' downwards!!!!!
   
   
      If Line2(alien_missile_check).Y2 > Image7(0).Top Then ' we may have hit a shield
       If Line2(alien_missile_check).Y2 < Image7(0).Top + Image7(0).Height Then ' it is looking more likely
      
       'find if shield hit.
       shield_loop = 0
       While shield_loop <= num_shields
        If shield_strength(shield_loop) >= 0 Then
      
        'Check for shield impact with living shield
         If Line2(alien_missile_check).X1 > Image7(shield_loop).Left Then
          If Line2(alien_missile_check).X1 < Image7(shield_loop).Left + Image7(shield_loop).Width Then
          
           'boom !
           ' kill the missile - job done
           Line2(alien_missile_check).Visible = False
           alien_missile(alien_missile_check) = False
           Line2(alien_missile_check).Y1 = 1
           Line2(alien_missile_check).Y2 = 1

           shield_strength(shield_loop) = shield_strength(shield_loop) - 1
           If shield_strength(shield_loop) < 0 Then
            Image7(shield_loop).Visible = False
           Else
           Image7(shield_loop).Top = Image7(shield_loop).Top + 15
            If shield_strength(shield_loop) < 5 Then
              Image7(shield_loop).Picture = Image6(5 - shield_strength(shield_loop)).Picture
            End If
           End If
          End If ' < left+impact
         End If ' > left
        End If ' shield alive
        shield_loop = shield_loop + 1
       Wend
       End If
      End If
   
   
   If Line2(alien_missile_check).Y2 > image2_bottom Then ' we have hit rock bottom
      alien_missile(alien_missile_check) = False
      Line2(alien_missile_check).Visible = False
      
      'Now the downer - you may have been hit ...
      If Not player_dying Then   ' Well would you kick a little icon when its been hit? - let the subsequent shots ride for a while..
       If Line2(alien_missile_check).X1 > Image2.Left Then
        If Line2(alien_missile_check).X1 < Image2.Left + player_width Then
         Line2(alien_missile_check).Visible = False
         player_dying = True
         
         moving_left = False   ' Well you are
         moving_right = False  ' not are you?
         
         'play the death sound...
         sndPlaySound App.Path + "\plr_dies.wav", 1
         
         Timer2.Interval = player_die_interval
         If lives_left > 0 Then Image3(lives_left - 1).Visible = False
         lives_left = lives_left - 1
        End If 'zap boom
       End If 'we have an impact
      End If
      Line2(alien_missile_check).Y1 = 1
      Line2(alien_missile_check).Y2 = 1
   End If  ' we have hit the bottom somewhere
 End If
 alien_missile_check = alien_missile_check + 1
Wend

End Sub


Private Sub timer5_Timer()
End
End Sub


