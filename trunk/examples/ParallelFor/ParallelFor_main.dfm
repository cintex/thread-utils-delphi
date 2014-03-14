object Form_main: TForm_main
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Parallel "for" example'
  ClientHeight = 244
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 110
  TextHeight = 14
  object Memo_log: TMemo
    Left = 0
    Top = 44
    Width = 333
    Height = 200
    Align = alBottom
    Lines.Strings = (
      'Memo_log')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Edt_Iterations: TSpinEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 23
    MaxValue = 200
    MinValue = 1
    TabOrder = 1
    Value = 10
  end
  object Btn_For: TButton
    Left = 144
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 2
    OnClick = Btn_ForClick
  end
  object Btn_Test: TButton
    Left = 240
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Btn_Test'
    TabOrder = 3
    Visible = False
  end
end
