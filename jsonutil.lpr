(********************************************************)
(*                                                      *)
(*  Json Util Pascal Program                            *)
(*  A small json parser with no dependencies            *)
(*                                                      *)
(*  http://www.getlazarus.org/json                      *)
(*  Licence GPLv3 released February 2020                *)
(*  see http://www.gnu.org/licenses/gpl-3.0.html        *)
(*                                                      *)
(********************************************************)

program jsonutil;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { Built-In }
  Classes,
  SysUtils,
  CustApp,
  LazUTF8,
  { 3rd Party }
  JsonTools
  { you can add units after this };

type

  { TJsonUtil }

  TJsonUtil = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteHelpFull;
  end;

{ TJsonUtil }

procedure TJsonUtil.DoRun;
var
  ErrorMsg: String;
  InPath: String;
  OutPath: String;
  OutStream: TFileStream;
  N: TJsonNode;
  Buffer: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions(
    'hi:o:pv',
    'help input: output: pretty version'
  );

  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then begin
    WriteHelpFull;
    Terminate;
    Exit;
  end;

  if HasOption('v', 'version') then begin
    WriteLn('Version: 0.0.1');
    Terminate;
    Exit;
  end;

  if HasOption('i', 'input') then begin
    InPath := GetOptionValue('i', 'input');
    N := TJsonNode.Create;
    N.LoadFromFile(InPath);

    if HasOption('p', 'pretty') then begin
      Buffer := N.Value;
    end
    else begin
      Buffer := N.AsJson;
    end;

    FreeAndNil(N);
  end
  else begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('o', 'output') then begin
    OutPath := GetOptionValue('o', 'output');
    OutStream := TFileStream.Create(OutPath, fmCreate);
    try
      OutStream.Position := 0;
      OutStream.Write(PChar(Buffer)^, Length(Buffer));
    finally
      FreeAndNil(OutStream);
    end;
  end
  else begin
    Write(Buffer);
  end;

  // stop program loop
  Terminate;
end;

constructor TJsonUtil.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TJsonUtil.Destroy;
begin
  inherited Destroy;
end;

procedure TJsonUtil.WriteHelp;
begin
  WriteLn('Usage: ', ExeName, ' -i input_path -o output_path -p');
  WriteLn('Use --help or -h flag to see full documentation');
end;

procedure TJsonUtil.WriteHelpFull;
begin
  WriteLn('Usage: ', ExeName, ' arguments');     
  WriteLn;
  WriteLn('Arguments:');   
  WriteLn;
  WriteLn('  --help');
  WriteLn('    alias -h');
  WriteLn('    Prints help.');
  WriteLn;
  WriteLn('  --input [input_path]');
  WriteLn('    alias -i'); 
  WriteLn('    Defines path to JSON file to read.');
  WriteLn;
  WriteLn('  --output [output_path]');
  WriteLn('    alias -o');   
  WriteLn('    Defines path to JSON file to write.');
  WriteLn;
  WriteLn('  --pretty');
  WriteLn('    alias -p');    
  WriteLn('    Sets output format to [pretty printing].');
  WriteLn;      
  WriteLn('  --version');
  WriteLn('    alias -v');
  WriteLn('    Prints program version.');
  WriteLn;
end;

var
  Application: TJsonUtil;

{$R *.res}

begin
  Application:=TJsonUtil.Create(nil);
  Application.Title:='Json Util';
  Application.Run;
  Application.Free;
end.

