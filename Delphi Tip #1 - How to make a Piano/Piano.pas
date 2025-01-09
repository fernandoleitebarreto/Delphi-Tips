unit Piano;

interface

uses
  System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,
  Winapi.Messages, MMSystem, Vcl.Dialogs, Vcl.Forms, System.SysUtils,
  System.Threading, System.Generics.Collections;

type
  TPiano = class;
  TPianoKeyColors = class;
  TThreadSQL = class;
  TKeyState = (ksLeave, ksEnter, ksPressed);
  TPianoStateChange = procedure(Sender: TPiano; State: TKeyState) of object;
  TNote = (pUnknown = 0, pDo, pDoSharp, pRe, pReSharp, pMi, pFa, pFaSharp, pSol,
    pSolSharp, pLa, pLaSharp, pSi);

  TNoteHelper = record helper for TNote
    function AsByte: byte;
    function ToString: string;
    function isSharp: Boolean;
    function Color: TColor;
    function Font: TColor;
    function Sound: string;
  end;

  TPianoKeyColors = class
  private
    FEnter, FDown: TColor;
  public
    property Enter: TColor read FEnter write FEnter;
    property Down: TColor read FDown write FDown;
  end;

  TThreadSQL = class(TThread)
  private
    { Private declarations }
    FEventoExecute: TProc;
    FEventoAfterExecute: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(pEventoExecute: TProc; pEventoAfterExecute: TProc = nil);
    destructor Destroy; override;

  end;

  TPiano = class(TPanel)
  private
    iContTasks: Integer;
    FNote: TNote;
    FColors: TPianoKeyColors;
    // FThreads: TObjectList<TThread>;
    FTasks: array of iTask;
    FOnStateChange: TPianoStateChange;
    FState: TKeyState;
    procedure SetNote(const Value: TNote);
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Note: TNote read FNote write SetNote;
    // property onMouseEnter;
    // property onMouseLeave;
    property onClick;
    property onStateChange: TPianoStateChange read FOnStateChange
      write FOnStateChange;

    property Colors: TPianoKeyColors read FColors write FColors;
    property State: TKeyState read FState write FState;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MyComponents', [TPiano]);
end;

{ TPiano }

procedure TPiano.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  FState := ksEnter;
  Paint;
  { Do Whatever }
end;

procedure TPiano.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  FState := ksLeave;
  Paint;
end;

constructor TPiano.Create(AOwner: TComponent);
begin
  inherited;
  // FThreads := TObjectList<TThread>.Create(True);
  SetLength(FTasks, 1);
  iContTasks := -1;
  FColors := TPianoKeyColors.Create;
  with FColors do
  begin
    Enter := clYellow;
    Down := clGray;
  end;
end;

destructor TPiano.Destroy;
begin
  FreeAndNil(FColors);
  // FreeAndNil(FThreads);
  // FreeAndNil(FTasks);

  inherited;
end;

procedure TPiano.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FState := ksPressed;
  Paint;
end;

procedure TPiano.Paint;
begin
  ParentBackground := False;
  inherited;

  if FState = ksLeave then
    Color := FNote.Color;
  if FState = ksEnter then
    Color := FColors.Enter;
  if FState = ksPressed then
  begin
    Inc(iContTasks);
    SetLength(FTasks, iContTasks + 1);

    FTasks[iContTasks] := TTask.Create(
      procedure()
      begin
        TThreadSQL.Create(
          procedure
          begin
            sndPlaySound(PChar(FNote.Sound), SND_NODEFAULT Or SND_ASYNC);
          end,
          procedure()
          begin
            Dec(iContTasks);
          end);
      end);
    FTasks[iContTasks].Start;

    TTask.WaitForAll(FTasks);


    // TThread.CreateAnonymousThread(
    // procedure
    // begin
    // sndPlaySound(PChar(FNote.Sound), SND_NODEFAULT Or SND_ASYNC);
    // end).Start;

    Color := FColors.Down;
    // AThread.FreeOnTerminate := False;
    // FThreads.Add(AThread); // so ARC won't throw FThreads away (it happened!)
    // ShowMessage(IntToStr(FThreads.Count));
    //
    // i := 1; // preserve the dummy thread reference
    // while i < FThreads.Count do
    // begin
    // AThread := FThreads.Items[i];
    // AThread.WaitFor;
    // Inc(i);
    // end;
    // // delete all but the dummy thread [0]
    // while FThreads.Count > 1 do
    // begin
    // i := FThreads.Count - 1;
    // FThreads.Delete(i);
    // FThreads.TrimExcess;
    // end;

    // TThread.CreateAnonymousThread(
    // procedure
    // begin
    // sndPlaySound(PChar(GetCurrentDir + '\PianoNotes\1-' + sNote + '.wav'),
    // SND_NODEFAULT Or SND_ASYNC);
    // end).Start;

    // TTask.WaitForAll(Tasks);

  end;
end;

procedure TPiano.SetNote(const Value: TNote);
begin
  FNote := Value;
  Caption := Value.ToString;
  Color := Value.Color;
  Font.Color := Value.Font;
end;

{ TNoteHelper }

function TNoteHelper.AsByte: byte;
begin
  Result := byte(Self);
end;

function TNoteHelper.Color: TColor;
begin
  if Self.isSharp then
    Result := clBlack
  else
    Result := clWindow;
end;

function TNoteHelper.Font: TColor;
begin
  if Self.isSharp then
    Result := clWhite
  else
    Result := clBlack;
end;

function TNoteHelper.isSharp: Boolean;
begin
  Result := Self in [pDoSharp, pReSharp, pFaSharp, pSolSharp, pLaSharp];
end;

function TNoteHelper.Sound: String;
begin
  Result := GetCurrentDir + '\PianoNotes\1-' + Self.ToString + '.wav';

end;

function TNoteHelper.ToString: string;
begin
  case Self of
    pDo:
      Result := 'C';
    pDoSharp:
      Result := 'C#';
    pRe:
      Result := 'D';
    pReSharp:
      Result := 'D#';
    pMi:
      Result := 'E';
    pFa:
      Result := 'F';
    pFaSharp:
      Result := 'F#';
    pSol:
      Result := 'G';
    pSolSharp:
      Result := 'G#';
    pLa:
      Result := 'A';
    pLaSharp:
      Result := 'A#';
    pSi:
      Result := 'B';
  end;
end;

{ TThreadSQL }

constructor TThreadSQL.Create(pEventoExecute, pEventoAfterExecute: TProc);
begin
  inherited Create(False);

  Self.FEventoExecute := pEventoExecute;
  Self.FEventoAfterExecute := pEventoAfterExecute;
  Self.FreeOnTerminate := True;
  // Quando terminar de rodar o Execute, já auto destroi
end;

destructor TThreadSQL.Destroy;
begin

  inherited;
end;

procedure TThreadSQL.Execute;
begin
  inherited;
  FEventoExecute;
  try
    Self.Synchronize(nil,
      procedure
      begin
        if (not Terminated) then
        begin
          if Assigned(FEventoAfterExecute) then
            FEventoAfterExecute;
        end; // if (not Terminated) then
      end);
  except
    on E: Exception do
    begin
      E.Message := 'Erro ao executar o Synchronize: ' + E.Message;
      raise;
    end;
  end;
end;

end.
