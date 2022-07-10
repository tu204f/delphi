unit Lb.Candel.Source;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils;

type
  ///<summary>
  /// Источние биржевых свечей
  ///</summary>
  TSourceCandel = class(TObject)
  private
    FSource: TCandelList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const ASource: TSourceCandel);
    // Загрузить файл из csv
    procedure SetLoadFile(const AFileName: String);
    // Превращаем в массив свечей
    procedure SetParserCandels(const ASource: TStrings);
    property Candels: TCandelList read FSource;
  public
    procedure GetMaxAndValue(var AValueMax, AValueMin: Double);
    procedure GetMaxAndValuePeriod(const ABeginIndex, ACount: Integer; var AValueMax, AValueMin: Double);
  end;

implementation

{ TSourceCandel }

procedure TSourceCandel.Assign(const ASource: TSourceCandel);
var
  i, Count: Integer;
begin
  FSource.Clear;
  if Assigned(ASource) then
  begin
    Count := ASource.Candels.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        var xC := ASource.Candels[i];
        Self.Candels.Add(xC);
      end;
  end;
end;

constructor TSourceCandel.Create;
begin
  FSource := TCandelList.Create;
end;

destructor TSourceCandel.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

procedure TSourceCandel.SetLoadFile(const AFileName: String);
var
  xStr: TStrings;
begin
  if FileExists(AFileName) then
    xStr := TStringList.Create;
    try
      xStr.Clear;
      xStr.LoadFromFile(AFileName);
      Self.SetParserCandels(xStr);
    finally
      FreeAndNil(xStr);
    end;
end;

procedure TSourceCandel.SetParserCandels(const ASource: TStrings);
var
  xCandel: TCandel;
begin
  FSource.Clear;
  if Assigned(ASource) then
    for var S in ASource do
    begin
      if not S.IsEmpty then
      begin
        if S.Chars[0] = '<' then
          Continue;
        xCandel := TCandel.Cretae(S);
        FSource.Add(xCandel);
      end
    end;
end;

procedure TSourceCandel.GetMaxAndValue(var AValueMax, AValueMin: Double);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  AValueMax := 0;
  AValueMin := 0;
  iCount := FSource.Count;
  if iCount > 0 then
  begin
    xCandel := FSource[0];
    AValueMax := xCandel.High;
    AValueMin := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FSource[i];
      if xCandel.High > AValueMax then
        AValueMax := xCandel.High;
      if xCandel.Low < AValueMin then
        AValueMin := xCandel.Low;
    end;
  end;
end;

procedure TSourceCandel.GetMaxAndValuePeriod(const ABeginIndex, ACount: Integer; var AValueMax, AValueMin: Double);
var
  xCandel: TCandel;
  i, iCount, xIndex: Integer;
begin
  AValueMax := 0;
  AValueMin := 0;
  iCount := FSource.Count;
  if iCount > 0 then
  begin
    xIndex := ABeginIndex;
    xCandel := FSource[xIndex];
    AValueMax := xCandel.High;
    AValueMin := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xIndex := i + ABeginIndex;
      xCandel := FSource[xIndex];
      if xCandel.High > AValueMax then
        AValueMax := xCandel.High;
      if xCandel.Low < AValueMin then
        AValueMin := xCandel.Low;
    end;
  end;
end;

end.
