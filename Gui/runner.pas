unit Runner;

interface

uses
  Classes, SysUtils, UTF8Process, StdCtrls, ExtCtrls, Options;

type
  TFinishReason = (
    frFinished,
    frStopped,
    frFailure
  );

  TFinishedEvent = procedure(Sender: TObject; Reason: TFinishReason) of object;
  TProgressEvent = procedure(Sender: TObject; Index: Integer) of object;

  { TRunner }

  TRunner = class
  private
    FProcess: TProcessUTF8;
    FTimer: TTimer;
    FOutputMemo: TCustomMemo;
    FOnFinished: TFinishedEvent;
    FOnProgress: TProgressEvent;

    FInputPos: Integer;
    FFailures: Integer;
    FOptions: TOptions;
    FRunning: Boolean;
    FStopped: Boolean;

    procedure ReadProcessOutput;
    procedure TimerTicked(Sender: TObject);
    procedure RunNextItem(IsFirstRun: Boolean = False);
    procedure Finish(Reason: TFinishReason);
  public
    constructor Create(AOutputMemo: TCustomMemo);
    destructor Destroy; override;

    procedure Run(AOptions: TOptions);
    procedure Stop;

    property IsRunning: Boolean read FRunning;
    property Failures: Integer read FFailures;
    property InputPos: Integer read FInputPos;
    property OnFinished: TFinishedEvent read FOnFinished write FOnFinished;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  Process, Dialogs;

{ TRunner }

constructor TRunner.Create(AOutputMemo: TCustomMemo);
begin
  // Unfortunatelly, we cannot use TAsyncProcess since it does not work reliably on all platforms
  FProcess := TProcessUTF8.Create(nil);
  FProcess.Options := [poUsePipes, {$IFDEF MSWINDOWS}poNoConsole,{$ENDIF} poStderrToOutPut];

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerTicked;

  FOutputMemo := AOutputMemo;
end;

destructor TRunner.Destroy;
begin
  FProcess.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TRunner.ReadProcessOutput;
var
  BufStr: string;
begin
  while FProcess.Output.NumBytesAvailable > 0 do
  begin
    SetLength(BufStr, FProcess.Output.NumBytesAvailable);
    FProcess.Output.Read(BufStr[1], Length(BufStr));
    FOutputMemo.Append(BufStr);
  end;
end;

procedure TRunner.TimerTicked(Sender: TObject);
begin
  ReadProcessOutput;
  if not FProcess.Running then
    RunNextItem;
end;

procedure TRunner.RunNextItem(IsFirstRun: Boolean);
begin
  if not IsFirstRun and (FProcess.ExitCode <> 0) then
    Inc(FFailures);

  Inc(FInputPos);

  if FInputPos >= FOptions.Files.Count then
  begin
    if FFailures = 0 then
      Finish(frFinished)
    else
      Finish(frFailure);

    Exit;
  end;

  if FStopped then
  begin
    Finish(frStopped);
    Exit;
  end;

  if Assigned(FOnProgress) then
    FOnProgress(Self, FInputPos);

  FOptions.ToCmdLineParameters(FProcess.Parameters, FInputPos);

  try
    FProcess.Execute;
  except
    on Ex: Exception do
    begin
      FOutputMemo.Append(Ex.ClassName + ': ' + Ex.Message);
      Dialogs.MessageDlg('Failed to execute Deskew',
        'Deskew command line executable failed to start. Check that it is in the correct location ' +
        'and has the right permissions.' + sLineBreak + sLineBreak +
        'Executable path used: ' + FProcess.Executable,
        mtError, [mbOK], '');

      Finish(frFailure);
      Exit;
    end;
  end;

  if IsFirstRun then
    FTimer.Enabled := True;
end;

procedure TRunner.Finish(Reason: TFinishReason);
begin
  FTimer.Enabled := False;
  FRunning := False;
  if Assigned(FOnFinished) then
    FOnFinished(Self, Reason);
end;

procedure TRunner.Run(AOptions: TOptions);
begin
  FInputPos := -1;
  FFailures := 0;
  FOptions := AOptions;
  FStopped := False;
  FRunning := True;

  FProcess.Executable := FOptions.EffectiveExecutablePath;
  RunNextItem(True);
end;

procedure TRunner.Stop;
begin
  FStopped := True;
end;

end.

