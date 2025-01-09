unit uScrollBarNoTabSheet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dxSkinsCore, dxSkinsDefaultPainters,
  dxBarBuiltInMenu, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxGroupBox, cxPC;

type
  TFrmScrollBoxTabSheet = class(TForm)
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    cxTabSheet2: TcxTabSheet;
    cxGroupBox1: TcxGroupBox;
    cxGroupBox2: TcxGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure CriaScrollBox;
    procedure CriaScrollBox_Teste;
  public
    { Public declarations }
  end;

var
  FrmScrollBoxTabSheet: TFrmScrollBoxTabSheet;

implementation

{$R *.dfm}
{ TForm1 }

procedure TFrmScrollBoxTabSheet.CriaScrollBox;
var
  I, J, S: Integer;
  _slComponents_TabSheet: TStringList;
  _TabSheet: TcxTabSheet;
  _VerticalScrollBox: array of TScrollBox;
begin
  _slComponents_TabSheet := TStringList.Create;
  SetLength(_VerticalScrollBox, 0);

  try
    S := -1;
    for I := 0 to ComponentCount - 1 do
      if (Components[I] is TcxTabSheet) then
      begin
        INC(S);
        _TabSheet := TcxTabSheet(Components[I]);
        SetLength(_VerticalScrollBox, Length(_VerticalScrollBox) + 1);

        _VerticalScrollBox[S] := TScrollBox.Create(_TabSheet);
        _VerticalScrollBox[S].Name := 'ScrollBoxVertical_' + (S.ToString);
        _VerticalScrollBox[S].Align := alClient;
        _VerticalScrollBox[S].AutoScroll := True;
        _VerticalScrollBox[S].Autosize := False;
        _VerticalScrollBox[S].tabstop := False;
        _VerticalScrollBox[S].BorderStyle := bsNone;
        _VerticalScrollBox[S].Color := $00F1F5F7;
        _VerticalScrollBox[S].Parent := _TabSheet;

        _slComponents_TabSheet.Clear;
        for J := 0 to _TabSheet.ControlCount - 1 do
          if (_TabSheet.Controls[J] is TControl) and
            (TControl(_TabSheet.Controls[J]).Parent = _TabSheet) and
            (_TabSheet.Controls[J].Name <> _VerticalScrollBox[S].Name) then
          begin
            _slComponents_TabSheet.Add(TControl(_TabSheet.Controls[J]).Name);
          end;

        for J := 0 to _slComponents_TabSheet.Count - 1 do
          TControl(FindComponent(_slComponents_TabSheet[J])).Parent :=
            _VerticalScrollBox[S];

      end; // if (Components[I] is TcxTabSheet) then

  finally
    if Assigned(_slComponents_TabSheet) then
      FreeAndNil(_slComponents_TabSheet);
  end;

end;

procedure TFrmScrollBoxTabSheet.CriaScrollBox_Teste;
var
  _VerticalScrollBox: TScrollBox;
begin
  _VerticalScrollBox := TScrollBox.Create(cxTabSheet1);

  _VerticalScrollBox.Parent := cxTabSheet1;
  _VerticalScrollBox.Name := 'ScrollBox1';
  _VerticalScrollBox.Align := alClient;
  _VerticalScrollBox.AutoScroll := True;
  _VerticalScrollBox.Autosize := False;
  _VerticalScrollBox.tabstop := False;
  _VerticalScrollBox.Color := $00F1F5F7;
  _VerticalScrollBox.BorderStyle := bsNone;

  cxGroupBox1.Parent := _VerticalScrollBox;

end;

procedure TFrmScrollBoxTabSheet.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  CriaScrollBox;
  // CriaScrollBox_Teste;
end;

end.
