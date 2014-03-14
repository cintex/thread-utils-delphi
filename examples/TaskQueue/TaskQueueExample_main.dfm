object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TTaskQueue example'
  ClientHeight = 292
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    415
    292)
  PixelsPerInch = 110
  TextHeight = 16
  object Memo_main: TMemo
    Left = 10
    Top = 10
    Width = 279
    Height = 272
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo_main')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 313
    Top = 10
    Width = 87
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    Caption = 'Add task'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 313
    Top = 54
    Width = 87
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Btn_WaitFor: TButton
    Left = 313
    Top = 101
    Width = 87
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akTop, akRight]
    Caption = 'Wait Empty'
    TabOrder = 3
    OnClick = Btn_WaitForClick
  end
end
