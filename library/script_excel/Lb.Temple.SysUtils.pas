unit Lb.Temple.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TValueString = class(TObject)
  private
    FCountRow: Integer;
    FValues: TStringList;
    FResults: TStringList;
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetCountResult: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property CountResult: Integer read GetCountResult;
    property Value: String read GetValue write SetValue;
  end;



implementation

{ TValueString }

constructor TValueString.Create;
begin
  FCountRow := 50;
  FValues := TStringList.Create;
  FResults:= TStringList.Create;
end;

destructor TValueString.Destroy;
begin
  FreeAndNil(FResults);
  FreeAndNil(FValues);
  inherited;
end;

function TValueString.GetValue: String;
begin
  Result := FValues.Text;
end;

procedure TValueString.SetValue(const Value: String);

  procedure SetSplitUp(S: String; AStr: TStrings);
  var
    tmpS: String;
    xC: Char;
  begin
    tmpS := '';
    AStr.Clear;
    if not S.IsEmpty then
    begin
      for xC in S do
      begin
        tmpS := tmpS + xC;
        if tmpS.Length >= FCountRow then
        begin
          AStr.Add(tmpS);
          tmpS := '';
        end;
      end;
      if not tmpS.IsEmpty then
        AStr.Add(tmpS);
    end;
  end;

var
  xS: String;
  xT: String;
  xStr: TStringList;
  i, Count: Integer;
begin
  FResults.Clear;
  FValues.Text := Value;
  if FValues.Count > 0 then
  begin
    xStr := TStringList.Create;
    try
      for xS in FValues do
      begin
        SetSplitUp(xS,xStr);
        Count := xStr.Count;
        if Count > 0 then
          for i := 0 to Count - 1 do
          begin
            xT := xStr[i];
            xT := Trim(xT);
            FResults.Add(xT);
          end;
      end;
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

function TValueString.GetCountResult: Integer;
begin
  Result := FResults.Count;
end;



end.
