object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'App1'
  ClientHeight = 264
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 61
    Height = 13
    Caption = 'Command ID'
  end
  object Label2: TLabel
    Left = 144
    Top = 56
    Width = 65
    Height = 13
    Caption = 'Integer Value'
  end
  object Label3: TLabel
    Left = 144
    Top = 104
    Width = 51
    Height = 13
    Caption = 'String text'
  end
  object Label4: TLabel
    Left = 144
    Top = 152
    Width = 62
    Height = 13
    Caption = 'Double value'
  end
  object Label5: TLabel
    Left = 144
    Top = 200
    Width = 37
    Height = 13
    Caption = 'Number'
  end
  object Label6: TLabel
    Left = 200
    Top = 200
    Width = 22
    Height = 13
    Caption = 'Text'
  end
  object BtnSendCmd: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 41
    Caption = 'Send Command'
    TabOrder = 0
    OnClick = BtnSendCmdClick
  end
  object EdID: TEdit
    Left = 144
    Top = 24
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 1
    Text = '1'
  end
  object BtnSendInteger: TButton
    Left = 8
    Top = 56
    Width = 121
    Height = 41
    Caption = 'Send Integer Value'
    TabOrder = 2
    OnClick = BtnSendIntegerClick
  end
  object EdInteger: TEdit
    Left = 144
    Top = 72
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 3
    Text = '123456'
  end
  object BtnSendString: TButton
    Left = 8
    Top = 104
    Width = 121
    Height = 41
    Caption = 'Send String'
    TabOrder = 4
    OnClick = BtnSendStringClick
  end
  object EdString: TEdit
    Left = 144
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'TEST STRING'
  end
  object BtnSendDouble: TButton
    Left = 8
    Top = 152
    Width = 121
    Height = 41
    Caption = 'Send Double'
    TabOrder = 6
    OnClick = BtnSendDoubleClick
  end
  object EdDouble: TEdit
    Left = 144
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '15.49'
  end
  object BtnSendRecord: TButton
    Left = 8
    Top = 200
    Width = 121
    Height = 41
    Caption = 'Send Record'
    TabOrder = 8
    OnClick = BtnSendRecordClick
  end
  object EdRecNumber: TEdit
    Left = 144
    Top = 216
    Width = 49
    Height = 21
    TabOrder = 9
    Text = '123'
  end
  object EdRecText: TEdit
    Left = 200
    Top = 216
    Width = 89
    Height = 21
    TabOrder = 10
    Text = 'RECORD TEXT'
  end
  object CkRecFlag: TCheckBox
    Left = 304
    Top = 216
    Width = 89
    Height = 17
    Caption = 'Boolean Flag'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object TA: TDzTalkApp
    AutoActivate = True
    AutoFind = True
    MyWindowName = 'WND_APP1'
    DestWindowName = 'WND_APP2'
    Left = 240
    Top = 40
  end
end
