unit Lb.Sources;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TCandel = record
    ID: Integer;
    DateTime: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Double;
    ValueRSI: Double;
    AvgValueRSI: Double;
    Momentum: Double;
  end;
  TCandelList = TList<TCandel>;

  TSources = class(TCandelList)
  private
    procedure SetParser(const ASource: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save(const AFileName: String);
  end;

type
  TBuySell = (bsBuy, bsSell);

  TTrade = record
    DateTime: TDateTime;
    Price: Double;
    Quantity: Double;
    BuySell: TBuySell;
  end;






implementation

{ TSources }

constructor TSources.Create;
begin
  inherited;

end;

destructor TSources.Destroy;
begin

  inherited;
end;

procedure TSources.Save(const AFileName: String);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile(AFileName);
    SetParser(xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TSources.SetParser(const ASource: TStrings);

  function _StrField(ALines: TStrings; AIndex: Integer): String;
  begin
    Result := '';
    if (AIndex >= 0) and (AIndex < ALines.Count) then
      Result := ALines[AIndex];
  end;

  function _GetParserToStr(S: String): TCandel;
  var
    xStr: TStrings;
    xCandel: TCandel;
  begin
    xStr := TStringList.Create;
    try
      xStr.Delimiter := ';';
      xStr.DelimitedText := S;
      with xCandel do
      begin
        ID          := StrToIntDef(_StrField(xStr,0),0);
        DateTime    := StrToTimeDef(_StrField(xStr,2),0);
        Open        := StrToFloatDef(_StrField(xStr,3),0);
        High        := StrToFloatDef(_StrField(xStr,4),0);
        Low         := StrToFloatDef(_StrField(xStr,5),0);
        Close       := StrToFloatDef(_StrField(xStr,6),0);
        Vol         := StrToFloatDef(_StrField(xStr,7),0);
        ValueRSI    := StrToFloatDef(_StrField(xStr,8),0);
        AvgValueRSI := StrToFloatDef(_StrField(xStr,9),0);
        Momentum    := StrToFloatDef(_StrField(xStr,10),0);
      end;
      Result := xCandel;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  xCandel: TCandel;
begin
  Self.Clear;
  for  var xS in ASource do
  begin
    xCandel := _GetParserToStr(xS);
    Self.Add(xCandel);
  end;
end;

end.
