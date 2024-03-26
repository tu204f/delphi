unit Lb.Params;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes;

type
  TStringParams = class(TStringList)
  private
    function GetAsString(ANameParam: String): String;
    function GetAsInteger(ANameParam: String): Integer;
    function GetAsDouble(ANameParam: String): Double;
    procedure SetAsString(ANameParam: String; const Value: String);
    procedure SetAsInteger(ANameParam: String; const Value: Integer);
    procedure SetAsDouble(ANameParam: String; const Value: Double);
  public
    procedure Save(const AFileName: String);
    procedure Load(const AFileName: String);
    property AsString[ANameParam: String]: String read GetAsString write SetAsString;
    property AsInteger[ANameParam: String]: Integer read GetAsInteger write SetAsInteger;
    property AsDouble[ANameParam: String]: Double read GetAsDouble write SetAsDouble;
  end;

implementation


{ TStringParams }

procedure TStringParams.Save(const AFileName: String);
var
  xFN: String;
begin
  xFN := ExtractFilePath(ParamStr(0)) + PathDelim + AFileName + '.prm';
  SaveToFile(xFN,TEncoding.UTF8);
end;

procedure TStringParams.Load(const AFileName: String);
var
  xFN: String;
begin
  xFN := ExtractFilePath(ParamStr(0)) + PathDelim + AFileName + '.prm';
  if FileExists(xFN) then
    LoadFromFile(xFN,TEncoding.UTF8);
end;

function TStringParams.GetAsString(ANameParam: String): String;
begin
  Result := Self.Values[ANameParam];
end;

procedure TStringParams.SetAsString(ANameParam: String; const Value: String);
begin
  Self.Values[ANameParam] := Value;
end;

function TStringParams.GetAsInteger(ANameParam: String): Integer;
begin
  Result := StrToIntDef(Self.AsString[ANameParam],0);
end;

procedure TStringParams.SetAsInteger(ANameParam: String; const Value: Integer);
begin
  Self.AsString[ANameParam] := IntToStr(Value);
end;

function TStringParams.GetAsDouble(ANameParam: String): Double;
begin
  Result := StrToFloatDef(Self.AsString[ANameParam],0);
end;

procedure TStringParams.SetAsDouble(ANameParam: String; const Value: Double);
var
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';
  Self.AsString[ANameParam] := FloatToStr(Value,xF);
end;

end.
