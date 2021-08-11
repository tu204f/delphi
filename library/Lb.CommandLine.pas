(*******************************************************************************
  Командная строка
*******************************************************************************)
unit Lb.CommandLine;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  System.SysUtils,
  System.Contnrs,
  System.Classes,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections;

type
  /// <summary>
  /// Команда
  /// </summary>
  TCommand = record
  public type
    TCommandType = (stString,stValue);
  private
    FValue: String;
    FName: String;
    FCommandType: TCommandType;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: String);
  public
    constructor Create(const AName, AValue: String);
    function ToString: String;
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property CommandType: TCommandType read FCommandType;
  end;

  /// <summary>
  /// Список команд
  /// </summary>
  TCommandList = TList<TCommand>;

  /// <summary>
  /// Список команд
  /// </summary>
  TCommandLine = class(TObject)
  private
    FCommands: TCommandList;
    function GetValue(Name: String): String;
  protected
    FCmdText: String;
    procedure SetParserCmd;
    procedure SetInitialization;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(const AName: String): Integer;
    property Value[Name: String]: String read GetValue;
    property Commands: TCommandList read FCommands;
    property CmdText: String read FCmdText;
  end;

function GetCommandLine: TCommandLine;
function GetCommandValue(const AName: String): String;
function GetCommandText: String;

implementation

uses
  Lb.Logger;

var
  CommandLine: TCommandLine = nil;

function GetCommandLine: TCommandLine;
begin
  if not Assigned(CommandLine) then
  begin
    CommandLine := TCommandLine.Create;
    CommandLine.SetInitialization;
  end;
  Result := CommandLine;
end;

function GetCommandValue(const AName: String): String;
var
  xCmd: TCommandLine;
begin
  Result := '';
  xCmd := GetCommandLine;
  if Assigned(xCmd) then
    Result := xCmd.Value[AName];
end;

function GetCommandText: String;
begin
  Result := GetCommandLine.CmdText;
end;

{ TCommand }

constructor TCommand.Create(const AName, AValue: String);
begin
  Self.Name := AName;
  Self.Value := AValue;
end;

procedure TCommand.SetName(const Value: String);
begin
  if not Value.IsEmpty then
    if Value[1] = '-' then    
      FName := Copy(Value,2,Value.Length)
    else
      FName := Value;
end;

procedure TCommand.SetValue(const Value: String);
begin
  if not Value.IsEmpty then
    if Value[1] = '"' then
    begin
      FCommandType := TCommandType.stString;
      FValue := Copy(Value,2,Value.Length - 2);
    end
    else
    begin
      FCommandType := TCommandType.stValue;
      FValue := Value;
    end;
end;

function TCommand.ToString: String;
begin
  Result := Name + ' = ' + Value;
end;

{ TCommandLine }

constructor TCommandLine.Create;
begin
  FCommands := TCommandList.Create;
end;

destructor TCommandLine.Destroy;
begin
  FreeAndNil(FCommands);
  inherited;
end;

function TCommandLine.GetValue(Name: String): String;
var
  xInd: Integer;
begin
  Result := '';
  xInd := IndexOf(Name);
  if xInd >= 0 then
    Result := FCommands[xInd].Value;
end;

procedure TCommandLine.SetInitialization;
begin
  FCommands.Clear;
  SetParserCmd;
end;

procedure TCommandLine.SetParserCmd;

  function GetToCommand(const Value: String): TCommand;
  var
    xInd: Integer;
  begin
    xInd := Pos(':',Value);
    Result.Name := Copy(Value,1,xInd - 1);
    Result.Value := Copy(Value,xInd + 1,Value.Length);
  end;

  procedure SetStringCmd(const AStrings: TStrings);
  var
    xS: String;
    xCommand: TCommand;
    i, Count: Integer;  
  begin
    Count := AStrings.Count;
    if Count > 0 then
    begin
      xS := AStrings[0];
      xCommand.Create('path_exe',xS);
      FCommands.Add(xCommand);
      for i := 1 to Count - 1 do
      begin
        xS := AStrings[i];
        if Pos(':',xS) > 0 then
        begin
          FCommands.Add(GetToCommand(xS));
        end
        else
        begin
          xCommand.Create('param_' + IntToStr(i),xS);
          FCommands.Add(xCommand);
        end; 
      end;
    end;
  end;


  procedure SetCmd;
  var
    xB: Boolean;
    xS, tmpS: String;
    i, L: Integer;
    xStr: TStrings;
  begin
    tmpS := '';
    xS := System.CmdLine;
    FCmdText := xS;
    try
      xB := False;
      xStr := TStringList.Create;
      xStr.Clear;
      L := xS.Length;
      if L > 0 then
        for i := 1 to L do
        begin
          if xS[i] = '"' then
            xB := not xB;
          if xB then
            tmpS := tmpS + xS[i]
          else
          begin
            if xS[i] = ' ' then
            begin
              if not tmpS.IsEmpty then
              begin
                xStr.Add(tmpS);
                tmpS := '';
              end;
            end
            else
              tmpS := tmpS + xS[i];
          end;
        end;                
      if not tmpS.IsEmpty then
        xStr.Add(tmpS);    
      SetStringCmd(xStr);
    finally
      FreeAndNil(xStr);
    end;
  end;

//var
//  xCommand: TCommand;
begin
  FCommands.Clear;
  // xCommand.Create('path_exe',ParamStr(0));
  // FCommands.Add(xCommand);
  SetCmd;
end;

procedure TCommandLine.Clear;
begin
  FCommands.Clear;
end;

function TCommandLine.IndexOf(const AName: String): Integer;
var
  xCommand: TCommand;
  i, Count: Integer;
begin
  Result := -1;
  Count := FCommands.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xCommand := FCommands[i];
      if SameText(xCommand.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;


// ----------------------------------------------------------------------------
procedure SetFinalizationCommandLine;
begin
  try
    if Assigned(CommandLine) then
      FreeAndNil(CommandLine);
    CommandLine := nil;
  except
    CommandLine := nil;
  end;
end;

initialization
//  GetCommandLine;
//  if not Assigned(CommandLine) then
//  begin
//    CommandLine := TCommandLine.Create;
//    CommandLine.SetInitialization;
//  end;

finalization
  SetFinalizationCommandLine;

end.
