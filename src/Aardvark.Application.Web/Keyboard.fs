namespace Aardvark.Application

open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base.Incremental
open System
open Aardvark.Base

type Keys =
    /// No key pressed.
    | None = 0

    /// The CANCEL key.
    | Cancel = 1

    /// The BACKSPACE key.
    | Back = 2

    /// The TAB key.
    | Tab = 3

    /// The LineFeed key.
    | LineFeed = 4

    /// The CLEAR key.
    | Clear = 5

    /// The RETURN key.
    | Return = 6

    /// The ENTER key.
    | Enter = 600 // = Return

    /// The PAUSE key.
    | Pause = 7

    /// The CAPS LOCK key.
    | Capital = 8

    /// The CAPS LOCK key.
    | CapsLock = 8

    /// The IME Kana mode key.
    | KanaMode = 9

    /// The IME Hangul mode key.
    | HangulMode = 9

    /// The IME Junja mode key.
    | JunjaMode = 10

    /// The IME Final mode key.
    | FinalMode = 11

    /// The IME Hanja mode key.
    | HanjaMode = 12

    /// The IME Kanji mode key.
    | KanjiMode = 12

    /// The ESC key.
    | Escape = 13

    /// The IME Convert key.
    | ImeConvert = 14

    /// The IME NonConvert key.
    | ImeNonConvert = 15

    /// The IME Accept key.
    | ImeAccept = 16

    /// The IME Mode change request.
    | ImeModeChange = 17

    /// The SPACEBAR key.
    | Space = 18

    /// The PAGE UP key.
    | Prior = 19

    /// The PAGE UP key.
    | PageUp = 19

    /// The PAGE DOWN key.
    | Next = 20

    /// The PAGE DOWN key.
    | PageDown = 20

    /// The END key.
    | End = 21

    /// The HOME key.
    | Home = 22

    /// The LEFT ARROW key.
    | Left = 23

    /// The UP ARROW key.
    | Up = 24

    /// The RIGHT ARROW key.
    | Right = 25

    /// The DOWN ARROW key.
    | Down = 26

    /// The SELECT key.
    | Select = 27

    /// The PRINT key.
    | Print = 28

    /// The EXECUTE key.
    | Execute = 29

    /// The PRINT SCREEN key.
    | Snapshot = 30

    /// The PRINT SCREEN key.
    | PrintScreen = 30

    /// The INS key.
    | Insert = 31

    /// The DEL key.
    | Delete = 32

    /// The HELP key.
    | Help = 33

    /// The 0 key.
    | D0 = 34

    /// The 1 key.
    | D1 = 35

    /// The 2 key.
    | D2 = 36

    /// The 3 key.
    | D3 = 37

    /// The 4 key.
    | D4 = 38

    /// The 5 key.
    | D5 = 39

    /// The 6 key.
    | D6 = 40

    /// The 7 key.
    | D7 = 41

    /// The 8 key.
    | D8 = 42

    /// The 9 key.
    | D9 = 43

    /// The A key.
    | A = 44

    /// The B key.
    | B = 45

    /// The C key.
    | C = 46

    /// The D key.
    | D = 47

    /// The E key.
    | E = 48

    /// The F key.
    | F = 49

    /// The G key.
    | G = 50

    /// The H key.
    | H = 51

    /// The I key.
    | I = 52

    /// The J key.
    | J = 53

    /// The K key.
    | K = 54

    /// The L key.
    | L = 55

    /// The M key.
    | M = 56

    /// The N key.
    | N = 57

    /// The O key.
    | O = 58

    /// The P key.
    | P = 59

    /// The Q key.
    | Q = 60

    /// The R key.
    | R = 61

    /// The S key.
    | S = 62

    /// The T key.
    | T = 63

    /// The U key.
    | U = 64

    /// The V key.
    | V = 65

    /// The W key.
    | W = 66

    /// The X key.
    | X = 67

    /// The Y key.
    | Y = 68

    /// The Z key.
    | Z = 69

    /// The left Windows logo key (Microsoft Natural Keyboard).
    | LWin = 70

    /// The right Windows logo key (Microsoft Natural Keyboard).
    | RWin = 71

    /// The Application key (Microsoft Natural Keyboard).
    | Apps = 72

    /// The Computer Sleep key.
    | Sleep = 73

    /// The 0 key on the numeric keypad.
    | NumPad0 = 74

    /// The 1 key on the numeric keypad.
    | NumPad1 = 75

    /// The 2 key on the numeric keypad.
    | NumPad2 = 76

    /// The 3 key on the numeric keypad.
    | NumPad3 = 77

    /// The 4 key on the numeric keypad.
    | NumPad4 = 78

    /// The 5 key on the numeric keypad.
    | NumPad5 = 79

    /// The 6 key on the numeric keypad.
    | NumPad6 = 80

    /// The 7 key on the numeric keypad.
    | NumPad7 = 81

    /// The 8 key on the numeric keypad.
    | NumPad8 = 82

    /// The 9 key on the numeric keypad.
    | NumPad9 = 83

    /// The Multiply key.
    | Multiply = 84

    /// The Add key.
    | Add = 85

    /// The Separator key.
    | Separator = 86

    /// The Subtract key.
    | Subtract = 87

    /// The Decimal key.
    | Decimal = 88

    /// The Divide key.
    | Divide = 89

    /// The F1 key.
    | F1 = 90

    /// The F2 key.
    | F2 = 91

    /// The F3 key.
    | F3 = 92

    /// The F4 key.
    | F4 = 93

    /// The F5 key.
    | F5 = 94

    /// The F6 key.
    | F6 = 95

    /// The F7 key.
    | F7 = 96

    /// The F8 key.
    | F8 = 97

    /// The F9 key.
    | F9 = 98

    /// The F10 key.
    | F10 = 99

    /// The F11 key.
    | F11 = 100

    /// The F12 key.
    | F12 = 101

    /// The F13 key.
    | F13 = 102

    /// The F14 key.
    | F14 = 103

    /// The F15 key.
    | F15 = 104

    /// The F16 key.
    | F16 = 105

    /// The F17 key.
    | F17 = 106

    /// The F18 key.
    | F18 = 107

    /// The F19 key.
    | F19 = 108

    /// The F20 key.
    | F20 = 109

    /// The F21 key.
    | F21 = 110

    /// The F22 key.
    | F22 = 111

    /// The F23 key.
    | F23 = 112

    /// The F24 key.
    | F24 = 113

    /// The NUM LOCK key.
    | NumLock = 114

    /// The SCROLL LOCK key.
    | Scroll = 115

    /// The left SHIFT key.
    | LeftShift = 116

    /// The right SHIFT key.
    | RightShift = 117

    /// The left CTRL key.
    | LeftCtrl = 118

    /// The right CTRL key.
    | RightCtrl = 119

    /// The left ALT key.
    | LeftAlt = 120

    /// The right ALT key.
    | RightAlt = 121

    /// The Browser Back key.
    | BrowserBack = 122

    /// The Browser Forward key.
    | BrowserForward = 123

    /// The Browser Refresh key.
    | BrowserRefresh = 124

    /// The Browser Stop key.
    | BrowserStop = 125

    /// The Browser Search key.
    | BrowserSearch = 126

    /// The Browser Favorites key.
    | BrowserFavorites = 127

    /// The Browser Home key.
    | BrowserHome = 128

    /// The Volume Mute key.
    | VolumeMute = 129

    /// The Volume Down key.
    | VolumeDown = 130

    /// The Volume Up key.
    | VolumeUp = 131

    /// The Media Next Track key.
    | MediaNextTrack = 132

    /// The Media Previous Track key.
    | MediaPreviousTrack = 133

    /// The Media Stop key.
    | MediaStop = 134

    /// The Media Play Pause key.
    | MediaPlayPause = 135

    /// The Launch Mail key.
    | LaunchMail = 136

    /// The Select Media key.
    | SelectMedia = 137

    /// The Launch Application1 key.
    | LaunchApplication1 = 138

    /// The Launch Application2 key.
    | LaunchApplication2 = 139

    /// The Oem 1 key.
    | Oem1 = 140

    /// The Oem Semicolon key.
    | OemSemicolon = 140

    /// The Oem plus key.
    | OemPlus = 141

    /// The Oem comma key.
    | OemComma = 142

    /// The Oem Minus key.
    | OemMinus = 143

    /// The Oem Period key.
    | OemPeriod = 144

    /// The Oem 2 key.
    | Oem2 = 145

    /// The Oem Question key.
    | OemQuestion = 145

    /// The Oem 3 key.
    | Oem3 = 146

    /// The Oem tilde key.
    | OemTilde = 146

    /// The ABNT_C1 (Brazilian) key.
    | AbntC1 = 147

    /// The ABNT_C2 (Brazilian) key.
    | AbntC2 = 148

    /// The Oem 4 key.
    | Oem4 = 149

    /// The Oem Open Brackets key.
    | OemOpenBrackets = 149

    /// The Oem 5 key.
    | Oem5 = 150

    /// The Oem Pipe key.
    | OemPipe = 150

    /// The Oem 6 key.
    | Oem6 = 151

    /// The Oem Close Brackets key.
    | OemCloseBrackets = 151

    /// The Oem 7 key.
    | Oem7 = 152

    /// The Oem Quotes key.
    | OemQuotes = 152

    /// The Oem8 key.
    | Oem8 = 153

    /// The Oem 102 key.
    | Oem102 = 154

    /// The Oem Backslash key.
    | OemBackslash = 154


    /// A special key masking the real key being processed by an IME.
    | ImeProcessed = 155

    /// A special key masking the real key being processed as a system key.
    | System = 156


    /// The OEM_ATTN key.
    | OemAttn = 157

    /// The DBE_ALPHANUMERIC key.
    | DbeAlphanumeric = 157

    /// The OEM_FINISH key.
    | OemFinish = 158

    /// The DBE_KATAKANA key.
    | DbeKatakana = 158

    /// The OEM_COPY key.
    | OemCopy = 159

    /// The DBE_HIRAGANA key.
    | DbeHiragana = 159

    /// The OEM_AUTO key.
    | OemAuto = 160

    /// The DBE_SBCSCHAR key.
    | DbeSbcsChar = 160

    /// The OEM_ENLW key.
    | OemEnlw = 161

    /// The DBE_DBCSCHAR key.
    | DbeDbcsChar = 161

    /// The OEM_BACKTAB key.
    | OemBackTab = 162

    /// The DBE_ROMAN key.
    | DbeRoman = 162

    /// The ATTN key.
    | Attn = 163

    /// The DBE_NOROMAN key.
    | DbeNoRoman = 163

    /// The CRSEL key.
    | CrSel = 164

    /// The DBE_ENTERWORDREGISTERMODE key.
    | DbeEnterWordRegisterMode = 164

    /// The EXSEL key.
    | ExSel = 165

    /// The DBE_ENTERIMECONFIGMODE key.
    | DbeEnterImeConfigureMode = 165

    /// The ERASE EOF key.
    | EraseEof = 166

    /// The DBE_FLUSHSTRING key.
    | DbeFlushString = 166

    /// The PLAY key.
    | Play = 167

    /// The DBE_CODEINPUT key.
    | DbeCodeInput = 167

    /// The ZOOM key.
    | Zoom = 168

    /// The DBE_NOCODEINPUT key.
    | DbeNoCodeInput = 168

    /// A constant reserved for future use.
    | NoName = 169

    /// The DBE_DETERMINESTRING key.
    | DbeDetermineString = 169

    /// The PA1 key.
    | Pa1 = 170

    /// The DBE_ENTERDLGCONVERSIONMODE key.
    | DbeEnterDialogConversionMode = 170

    /// The CLEAR key.
    | OemClear = 171

    /// Indicates the key is part of a dead-key composition
    | DeadCharProcessed = 172


module Keys =
    let internal ofList (entries : list<'a * 'b>) = 
        let d = System.Collections.Generic.Dictionary<'a, 'b>()
        for (k,v) in entries do d.[k] <- v
        d

    
    let internal keyTable =
        ofList [
            "alt",          Keys.LeftAlt
            "altgraph",     Keys.RightAlt
            "capslock",     Keys.CapsLock
            "control",      Keys.LeftCtrl
            //"fn",           Keys
            //"fnlock", Keys
            //"hyper"
            "meta",         Keys.LWin
            "numlock",      Keys.NumLock
            "scrollock",    Keys.Scroll
            "shift",        Keys.LeftShift
            //"super"
            //"symbol"
            //"symbollock"
            "enter",        Keys.Return
            "tab",          Keys.Tab
            " ",            Keys.Space

            "arrowdown",    Keys.Down
            "arrowleft",    Keys.Left
            "arrowright",   Keys.Right
            "arrowup",      Keys.Up
            "end",          Keys.End
            "home",         Keys.Home
            "pagedown",     Keys.PageDown
            "pageup",       Keys.PageUp

            "backspace",    Keys.Back
            "clear",        Keys.Clear
            "copy",         Keys.OemCopy
            //"cut",          Keys.Cut
            "crsel",        Keys.CrSel
            "delete",       Keys.Delete
            "eraseeof",     Keys.EraseEof
            "exsel",        Keys.ExSel
            "insert",       Keys.Insert
            "paste",        Keys.Insert
            //"redo",         Keys.
            //"undo",         Keys.

            "accept",       Keys.ImeAccept
            //"again",        Keys.Ag
            "attn",         Keys.Attn
            "cancel",       Keys.Cancel
            //"contextmenu",  Keys.Con
            "escape",       Keys.Escape
            "execute",      Keys.Execute
            "find",         Keys.BrowserSearch
            "finish",       Keys.OemFinish
            "help",         Keys.Help

            "pause",        Keys.Pause
            "play",         Keys.Play
            //"props",        Keys.Props
            "select",       Keys.Select
            "zoomin",       Keys.Zoom
            "zoomout",      Keys.Zoom
            //"dead",         Keys.Ti
            "printscreen",  Keys.PrintScreen

            "hangulmode",   Keys.HangulMode
            "hanjamode",    Keys.HanjaMode
            "junjamode",    Keys.JunjaMode

            "f1", Keys.F1
            "f2", Keys.F2
            "f3", Keys.F3
            "f4", Keys.F4
            "f5", Keys.F5
            "f6", Keys.F6
            "f7", Keys.F7
            "f8", Keys.F8
            "f9", Keys.F9
            "f10", Keys.F10
            "f11", Keys.F11
            "f12", Keys.F12
            "f13", Keys.F13
            "f14", Keys.F14
            "f15", Keys.F15
            "f16", Keys.F16
            "f17", Keys.F17
            "f18", Keys.F18
            "f19", Keys.F19
            "f20", Keys.F20
            "f21", Keys.F21
            "f22", Keys.F22
            "f23", Keys.F23
            "f24", Keys.F24

            "decimal", Keys.Decimal
            "multiply", Keys.Multiply
            "add", Keys.Add
            "clear", Keys.Clear
            "divide", Keys.Divide
            "subtract", Keys.Subtract
            "separator", Keys.Separator

            
            "0", Keys.D0
            "1", Keys.D1
            "2", Keys.D2
            "3", Keys.D3
            "4", Keys.D4
            "5", Keys.D5
            "6", Keys.D6
            "7", Keys.D7
            "8", Keys.D8
            "9", Keys.D9


            "a", Keys.A
            "b", Keys.B
            "c", Keys.C
            "d", Keys.D
            "e", Keys.E
            "f", Keys.F
            "g", Keys.G
            "h", Keys.H
            "i", Keys.I
            "j", Keys.J
            "k", Keys.K
            "l", Keys.L
            "m", Keys.M
            "n", Keys.N
            "o", Keys.O
            "p", Keys.P
            "q", Keys.Q
            "r", Keys.R
            "s", Keys.S
            "t", Keys.T
            "u", Keys.U
            "v", Keys.V
            "w", Keys.W
            "x", Keys.X
            "y", Keys.Y
            "z", Keys.Z

            
            //"-", Keys.OemMinus
            //"+", Keys.OemPlus
            //".", Keys.OemPeriod
            //",", Keys.OemComma
            //"*", Keys.Mu
            //"<", Keys.




            //"dead", Keys.
        ]

    let internal table =
        ofList [
            3, Keys.Pause
            8, Keys.Back
            9, Keys.Tab
            12, Keys.Clear
            13, Keys.Return
            16, Keys.LeftShift
            17, Keys.LeftCtrl
            18, Keys.LeftAlt
            19, Keys.Pause
            20, Keys.CapsLock
            21, Keys.HangulMode
            25, Keys.HanjaMode
            27, Keys.Escape
            28, Keys.ImeConvert
            29, Keys.ImeNonConvert
            32, Keys.Space
            33, Keys.PageUp
            34, Keys.PageDown
            35, Keys.End
            36, Keys.Home
            37, Keys.Left
            38, Keys.Up
            39, Keys.Right
            40, Keys.Down
            41, Keys.Select
            42, Keys.Print
            43, Keys.Execute
            44, Keys.PrintScreen
            45, Keys.Insert
            46, Keys.Delete
            47, Keys.Help

            48, Keys.D0
            49, Keys.D1
            50, Keys.D2
            51, Keys.D3
            52, Keys.D4
            53, Keys.D5
            54, Keys.D6
            55, Keys.D7
            56, Keys.D8
            57, Keys.D9

            //58, :
            59, Keys.OemSemicolon
            //60, <
            //61, equal
            //63, ß
            //64, @

            65, Keys.A
            66, Keys.B
            67, Keys.C
            68, Keys.D
            69, Keys.E
            70, Keys.F
            71, Keys.G
            72, Keys.H
            73, Keys.I
            74, Keys.J
            75, Keys.K
            76, Keys.L
            77, Keys.M
            78, Keys.N
            79, Keys.O
            80, Keys.P
            81, Keys.Q
            82, Keys.R
            83, Keys.S
            84, Keys.T
            85, Keys.U
            86, Keys.V
            87, Keys.W
            88, Keys.X
            89, Keys.Y
            90, Keys.Z

            91, Keys.LWin
            92, Keys.RWin
            93, Keys.RWin

            95, Keys.Sleep
            
            96, Keys.NumPad0
            97, Keys.NumPad1
            98, Keys.NumPad2
            99, Keys.NumPad3
            100, Keys.NumPad4
            101, Keys.NumPad5
            102, Keys.NumPad6
            103, Keys.NumPad7
            104, Keys.NumPad8
            105, Keys.NumPad9

            106, Keys.Multiply
            107, Keys.Add
            108, Keys.OemPeriod
            109, Keys.Subtract
            110, Keys.Decimal
            111, Keys.Divide

            112, Keys.F1
            113, Keys.F2
            114, Keys.F3
            115, Keys.F4
            116, Keys.F5
            117, Keys.F6
            118, Keys.F7
            119, Keys.F8
            120, Keys.F9
            121, Keys.F10
            122, Keys.F11
            123, Keys.F12
            124, Keys.F13
            125, Keys.F14
            126, Keys.F15
            127, Keys.F16
            128, Keys.F17
            129, Keys.F18
            130, Keys.F19
            131, Keys.F20
            132, Keys.F21
            133, Keys.F22
            134, Keys.F23
            135, Keys.F24
            
            144, Keys.NumLock
            145, Keys.Scroll
            // 160, '^'
            // 161, '!'
            // 162, '؛ (arabic semicolon)'
            // 163, '#'
            // 164, '$'
            // 165, 'ù'
            166, Keys.BrowserBack
            167, Keys.BrowserForward
            168, Keys.BrowserRefresh
            169, Keys.OemCloseBrackets
            171, Keys.OemTilde
            172, Keys.Home

            186, Keys.OemSemicolon
            187, Keys.OemPlus
            188, Keys.OemComma
            189, Keys.OemMinus
            190, Keys.OemPeriod
            194, Keys.OemPeriod
        ]

    let internal names =
        ofList [
            Keys.None, "None"
            Keys.Cancel, "Cancel"
            Keys.Back, "Back"
            Keys.Tab, "Tab"
            Keys.LineFeed, "LineFeed"
            Keys.Clear, "Clear"
            Keys.Pause, "Pause"
            Keys.Return, "Return"
            Keys.Enter, "Enter"
            Keys.Capital, "Capital"
            Keys.CapsLock, "CapsLock"
            Keys.KanaMode, "KanaMode"
            Keys.HangulMode, "HangulMode"
            Keys.JunjaMode, "JunjaMode"
            Keys.FinalMode, "FinalMode"
            Keys.HanjaMode, "HanjaMode"
            Keys.KanjiMode, "KanjiMode"
            Keys.Escape, "Escape"
            Keys.ImeConvert, "ImeConvert"
            Keys.ImeNonConvert, "ImeNonConvert"
            Keys.ImeAccept, "ImeAccept"
            Keys.ImeModeChange, "ImeModeChange"
            Keys.Space, "Space"
            Keys.Prior, "Prior"
            Keys.PageUp, "PageUp"
            Keys.Next, "Next"
            Keys.PageDown, "PageDown"
            Keys.End, "End"
            Keys.Home, "Home"
            Keys.Left, "Left"
            Keys.Up, "Up"
            Keys.Right, "Right"
            Keys.Down, "Down"
            Keys.Select, "Select"
            Keys.Print, "Print"
            Keys.Execute, "Execute"
            Keys.Snapshot, "Snapshot"
            Keys.PrintScreen, "PrintScreen"
            Keys.Insert, "Insert"
            Keys.Delete, "Delete"
            Keys.Help, "Help"
            Keys.D0, "D0"
            Keys.D1, "D1"
            Keys.D2, "D2"
            Keys.D3, "D3"
            Keys.D4, "D4"
            Keys.D5, "D5"
            Keys.D6, "D6"
            Keys.D7, "D7"
            Keys.D8, "D8"
            Keys.D9, "D9"
            Keys.A, "A"
            Keys.B, "B"
            Keys.C, "C"
            Keys.D, "D"
            Keys.E, "E"
            Keys.F, "F"
            Keys.G, "G"
            Keys.H, "H"
            Keys.I, "I"
            Keys.J, "J"
            Keys.K, "K"
            Keys.L, "L"
            Keys.M, "M"
            Keys.N, "N"
            Keys.O, "O"
            Keys.P, "P"
            Keys.Q, "Q"
            Keys.R, "R"
            Keys.S, "S"
            Keys.T, "T"
            Keys.U, "U"
            Keys.V, "V"
            Keys.W, "W"
            Keys.X, "X"
            Keys.Y, "Y"
            Keys.Z, "Z"
            Keys.LWin, "LWin"
            Keys.RWin, "RWin"
            Keys.Apps, "Apps"
            Keys.Sleep, "Sleep"
            Keys.NumPad0, "NumPad0"
            Keys.NumPad1, "NumPad1"
            Keys.NumPad2, "NumPad2"
            Keys.NumPad3, "NumPad3"
            Keys.NumPad4, "NumPad4"
            Keys.NumPad5, "NumPad5"
            Keys.NumPad6, "NumPad6"
            Keys.NumPad7, "NumPad7"
            Keys.NumPad8, "NumPad8"
            Keys.NumPad9, "NumPad9"
            Keys.Multiply, "Multiply"
            Keys.Add, "Add"
            Keys.Separator, "Separator"
            Keys.Subtract, "Subtract"
            Keys.Decimal, "Decimal"
            Keys.Divide, "Divide"
            Keys.F1, "F1"
            Keys.F2, "F2"
            Keys.F3, "F3"
            Keys.F4, "F4"
            Keys.F5, "F5"
            Keys.F6, "F6"
            Keys.F7, "F7"
            Keys.F8, "F8"
            Keys.F9, "F9"
            Keys.F10, "F10"
            Keys.F11, "F11"
            Keys.F12, "F12"
            Keys.F13, "F13"
            Keys.F14, "F14"
            Keys.F15, "F15"
            Keys.F16, "F16"
            Keys.F17, "F17"
            Keys.F18, "F18"
            Keys.F19, "F19"
            Keys.F20, "F20"
            Keys.F21, "F21"
            Keys.F22, "F22"
            Keys.F23, "F23"
            Keys.F24, "F24"
            Keys.NumLock, "NumLock"
            Keys.Scroll, "Scroll"
            Keys.LeftShift, "LeftShift"
            Keys.RightShift, "RightShift"
            Keys.LeftCtrl, "LeftCtrl"
            Keys.RightCtrl, "RightCtrl"
            Keys.LeftAlt, "LeftAlt"
            Keys.RightAlt, "RightAlt"
            Keys.BrowserBack, "BrowserBack"
            Keys.BrowserForward, "BrowserForward"
            Keys.BrowserRefresh, "BrowserRefresh"
            Keys.BrowserStop, "BrowserStop"
            Keys.BrowserSearch, "BrowserSearch"
            Keys.BrowserFavorites, "BrowserFavorites"
            Keys.BrowserHome, "BrowserHome"
            Keys.VolumeMute, "VolumeMute"
            Keys.VolumeDown, "VolumeDown"
            Keys.VolumeUp, "VolumeUp"
            Keys.MediaNextTrack, "MediaNextTrack"
            Keys.MediaPreviousTrack, "MediaPreviousTrack"
            Keys.MediaStop, "MediaStop"
            Keys.MediaPlayPause, "MediaPlayPause"
            Keys.LaunchMail, "LaunchMail"
            Keys.SelectMedia, "SelectMedia"
            Keys.LaunchApplication1, "LaunchApplication1"
            Keys.LaunchApplication2, "LaunchApplication2"
            Keys.Oem1, "Oem1"
            Keys.OemSemicolon, "OemSemicolon"
            Keys.OemPlus, "OemPlus"
            Keys.OemComma, "OemComma"
            Keys.OemMinus, "OemMinus"
            Keys.OemPeriod, "OemPeriod"
            Keys.Oem2, "Oem2"
            Keys.OemQuestion, "OemQuestion"
            Keys.Oem3, "Oem3"
            Keys.OemTilde, "OemTilde"
            Keys.AbntC1, "AbntC1"
            Keys.AbntC2, "AbntC2"
            Keys.Oem4, "Oem4"
            Keys.OemOpenBrackets, "OemOpenBrackets"
            Keys.Oem5, "Oem5"
            Keys.OemPipe, "OemPipe"
            Keys.Oem6, "Oem6"
            Keys.OemCloseBrackets, "OemCloseBrackets"
            Keys.Oem7, "Oem7"
            Keys.OemQuotes, "OemQuotes"
            Keys.Oem8, "Oem8"
            Keys.Oem102, "Oem102"
            Keys.OemBackslash, "OemBackslash"
            Keys.ImeProcessed, "ImeProcessed"
            Keys.System, "System"
            Keys.OemAttn, "OemAttn"
            Keys.DbeAlphanumeric, "DbeAlphanumeric"
            Keys.OemFinish, "OemFinish"
            Keys.DbeKatakana, "DbeKatakana"
            Keys.OemCopy, "OemCopy"
            Keys.DbeHiragana, "DbeHiragana"
            Keys.OemAuto, "OemAuto"
            Keys.DbeSbcsChar, "DbeSbcsChar"
            Keys.OemEnlw, "OemEnlw"
            Keys.DbeDbcsChar, "DbeDbcsChar"
            Keys.OemBackTab, "OemBackTab"
            Keys.DbeRoman, "DbeRoman"
            Keys.Attn, "Attn"
            Keys.DbeNoRoman, "DbeNoRoman"
            Keys.CrSel, "CrSel"
            Keys.DbeEnterWordRegisterMode, "DbeEnterWordRegisterMode"
            Keys.ExSel, "ExSel"
            Keys.DbeEnterImeConfigureMode, "DbeEnterImeConfigureMode"
            Keys.EraseEof, "EraseEof"
            Keys.DbeFlushString, "DbeFlushString"
            Keys.Play, "Play"
            Keys.DbeCodeInput, "DbeCodeInput"
            Keys.Zoom, "Zoom"
            Keys.DbeNoCodeInput, "DbeNoCodeInput"
            Keys.NoName, "NoName"
            Keys.DbeDetermineString, "DbeDetermineString"
            Keys.Pa1, "Pa1"
            Keys.DbeEnterDialogConversionMode, "DbeEnterDialogConversionMode"
            Keys.OemClear, "OemClear"
            Keys.DeadCharProcessed, "DeadCharProcessed"
        
        ]

    let getName (key : Keys) =
        if names.ContainsKey key then names.[key]
        else string key

    let ofEvent (e : KeyboardEvent) = 
        let key = e.key.ToLower()
        if keyTable.ContainsKey key then
            let c = keyTable.[key]
            if e.location = 2.0 then
                match c with
                | Keys.LWin -> Keys.RWin
                | Keys.LeftAlt -> Keys.RightAlt
                | Keys.LeftCtrl -> Keys.RightCtrl
                | Keys.LeftShift -> Keys.RightShift
                | _ -> c
            elif e.location = 3.0 then
                match c with
                | Keys.Return -> Keys.Enter
                | Keys.D0 -> Keys.NumPad0
                | Keys.D1 -> Keys.NumPad1
                | Keys.D2 -> Keys.NumPad2
                | Keys.D3 -> Keys.NumPad3
                | Keys.D4 -> Keys.NumPad4
                | Keys.D5 -> Keys.NumPad5
                | Keys.D6 -> Keys.NumPad6
                | Keys.D7 -> Keys.NumPad7
                | Keys.D8 -> Keys.NumPad8
                | Keys.D9 -> Keys.NumPad9
                | _ -> c
            else
                c
        else
            let code = e.keyCode
            if table.ContainsKey (int code) then
                let c = table.[int code]
                if e.location = 2.0 then
                    match c with
                    | Keys.LWin -> Keys.RWin
                    | Keys.LeftAlt -> Keys.RightAlt
                    | Keys.LeftCtrl -> Keys.RightCtrl
                    | Keys.LeftShift -> Keys.RightShift
                    | _ -> c
                elif e.location = 3.0 then
                    match c with
                    | Keys.Return -> Keys.Enter
                    | _ -> c
                else
                    c
            else
                Keys.None


type KeyboardAction =
    | Down of Keys
    | Up of Keys
    | Press of string

type IKeyboard =
    abstract member Actions : IObservable<KeyboardAction>
    abstract member IsDown : Keys -> IMod<bool>
    abstract member KeyDown : Keys -> IObservable<unit>
    abstract member KeyUp : Keys -> IObservable<unit>

    abstract member Down : IObservable<Keys>
    abstract member DownWithRepeats : IObservable<Keys>
    abstract member Up : IObservable<Keys>
    abstract member Press : IObservable<string>

type Keyboard(c : HTMLElement) =
    
    
    static let createHandler (handler : 'e -> unit) = U2.Case2 { new EventListenerObject with member x.handleEvent e = let e = unbox<'e> e in handler e }
    
    let downKeys =  System.Collections.Generic.HashSet<Keys>()
    let keyStates =  System.Collections.Generic.Dictionary<Keys, ModRef<bool>>()

    let getState (k : Keys) =
        if keyStates.ContainsKey k then
            keyStates.[k]
        else
            let m = Mod.init (downKeys.Contains k)
            keyStates.[k] <- m
            m

    


    let down = new Subject<Keys>()
    let downWithRepeats =  new Subject<Keys>()
    let up = new Subject<Keys>()
    let press = new Subject<string>()


    let events =
        lazy (
            let s = new Subject<KeyboardAction>()


            down.Subscribe(fun b -> s.OnNext(KeyboardAction.Down(b))) |> ignore
            up.Subscribe(fun b -> s.OnNext(KeyboardAction.Up(b))) |> ignore
            press.Subscribe(fun b -> s.OnNext(KeyboardAction.Press(b))) |> ignore

            s
        )

    let pressHandler = 
        createHandler (fun (e : KeyboardEvent) -> 
            press.OnNext e.key
            e.stopPropagation()
            e.preventDefault()
        )
    
    let downHandler = 
        createHandler (fun (e : KeyboardEvent) -> 
            let key = Keys.ofEvent e
            if key <> Keys.None then
                if not e.repeat then 
                    downKeys.Add key |> ignore
                
                    if keyStates.ContainsKey key then
                        transact (fun () -> keyStates.[key].Value <- true)

                    down.OnNext key
                downWithRepeats.OnNext key
        )

    let upHandler = 
        createHandler (fun (e : KeyboardEvent) ->   
            let key = Keys.ofEvent e
            if key <> Keys.None then
                downKeys.Remove key |> ignore

            
                if keyStates.ContainsKey key then
                    transact (fun () -> keyStates.[key].Value <- false)

                up.OnNext key
                //console.warn (sprintf "up: %A" (Keys.getName (Keys.ofEvent e)))
            e.stopPropagation(); e.preventDefault()
        )

    do
        c.focus()
        c.addEventListener("keypress", pressHandler, true)
        c.addEventListener("keydown", downHandler, true)
        c.addEventListener("keyup", upHandler, true)

    member x.Dispose() =
        c.removeEventListener("keypress", pressHandler, true)
        c.removeEventListener("keydown", downHandler, true)
        c.removeEventListener("keyup", upHandler, true)

    member x.Actions = events.Value :> IObservable<_>
    member x.Down = down :> IObservable<_>
    member x.DownWithRepeats = downWithRepeats :> IObservable<_>
    member x.Up = up :> IObservable<_>
    member x.IsDown a = getState a :> IMod<_>
    member x.KeyDown key = down |> Observable.choose (fun k -> if k = key then Some () else None)
    member x.KeyUp key = up |> Observable.choose (fun k -> if k = key then Some () else None)
    member x.Press = press :> IObservable<_>

    interface IKeyboard with
        member this.Actions = this.Actions
        member this.Down = this.Down
        member this.DownWithRepeats = this.DownWithRepeats
        member this.IsDown a = this.IsDown a
        member this.KeyDown key = down |> Observable.choose (fun k -> if k = key then Some () else None)
        member this.KeyUp key = up |> Observable.choose (fun k -> if k = key then Some () else None)
        member this.Press = press :> IObservable<_>
        member this.Up = up :> IObservable<_>
