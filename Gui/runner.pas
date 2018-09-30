unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsyncProcess, StdCtrls, Options;

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
    FProcess: TAsyncProcess;
    FOutputMemo: TCustomMemo;
    FOnFinished: TFinishedEvent;
    FOnProgress: TProgressEvent;

    FInputPos: Integer;
    FFailures: Integer;
    FOptions: TOptions;
    FRunning: Boolean;
    FStopped: Boolean;

    procedure ProcessReadData(Sender: TObject);
    procedure ProcessTerminate(Sender: TObject);

    procedure RunNextItem(CheckLastResult: Boolean);
    procedure Finish(Reason: TFinishReason);
  public
    constructor Create(AProcess: TAsyncProcess; AOutputMemo: TCustomMemo);

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
  Process;

{ TRunner }

constructor TRunner.Create(AProcess: TAsyncProcess; AOutputMemo: TCustomMemo);
begin
  FProcess := AProcess;
  FProcess.Options := [poUsePipes, poNoConsole, poStderrToOutPut];
  FProcess.OnReadData := @ProcessReadData;
  FProcess.OnTerminate := @ProcessTerminate;

  FOutputMemo := AOutputMemo;
end;

procedure TRunner.ProcessReadData(Sender: TObject);
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

procedure TRunner.ProcessTerminate(Sender: TObject);
begin
  // Check on any leftover ouput. If program terminates fast enough
  // they may not be even one OnReadData event.
  ProcessReadData(nil);

  RunNextItem(True);
end;

procedure TRunner.RunNextItem(CheckLastResult: Boolean);
begin
  if CheckLastResult and (FProcess.ExitCode <> 0) then
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

  if FOnProgress <> nil then
    FOnProgress(Self, FInputPos);

  FOptions.ToCmdLineParameters(FProcess.Parameters, FInputPos);
  FProcess.Execute;
end;

procedure TRunner.Finish(Reason: TFinishReason);
begin
  FRunning := False;
  if FOnFinished <> nil then
    FOnFinished(Self, Reason);
end;

procedure TRunner.Run(AOptions: TOptions);
begin
  FInputPos := -1;
  FFailures := 0;
  FOptions := AOptions;
  FStopped := False;
  FRunning := True;

  FProcess.Executable := FOptions.ExecutablePath;
  RunNextItem(False);
end;

procedure TRunner.Stop;
begin
  FStopped := True;
end;

end.

