object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'App2'
  ClientHeight = 264
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object M: TMemo
    Left = 8
    Top = 8
    Width = 433
    Height = 249
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object TA: TDzTalkApp
    AutoActivate = True
    AutoFind = True
    MyWindowName = 'WND_APP2'
    DestWindowName = 'WND_APP1'
    OnMessage = TAMessage
    Left = 224
    Top = 64
  end
end
