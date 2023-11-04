unit Lb.Block;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.NeuronNet,
  Lb.SysUtils.Candel;

type
  TBlock = class;
  TBlockList = TObjectList<TBlock>;

  ///<summary>Базовый объект</summary>
  TBlock = class(TObject)
  private
    FCandels: TCandels;
    FUneCandels: TCandels;
  private
    procedure SetConvert(ACandels: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFileName(const AFileName: String);
    procedure SetParserBlock(const ASources: TStrings);
    property Candels: TCandels read FCandels;
    property UneCandels: TCandels read FUneCandels;
  end;

implementation

uses
  System.Threading;

{ TBlock }

constructor TBlock.Create;
begin
  FCandels := TCandels.Create;
  FUneCandels := TCandels.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FUneCandels);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TBlock.LoadFileName(const AFileName: String);
var
  xSources: TStrings;
begin
  xSources := TStringList.Create;
  try
    xSources.LoadFromFile(AFileName);
    SetParserBlock(xSources);
  finally
    FreeAndNil(xSources);
  end;
end;

procedure TBlock.SetParserBlock(const ASources: TStrings);
var
  xCandel: TCandel;
  i, iCount: Integer;
  xS: String;
begin
  iCount := ASources.Count;
  if iCount > 0 then
  begin

    for i := 0 to iCount - 1 do
    begin
      xS := ASources[i];
      xCandel.Create(xS);
      FCandels.Add(xCandel);
    end;

  end;
  SetConvert(FCandels);
end;

procedure TBlock.SetConvert(ACandels: TCandelList);

  procedure _SetMaxAndMinPrice(var AMaxPrice, AMinPrice: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    // Предельное значение цены
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AMaxPrice := xCandel.High;
      AMinPrice := xCandel.Low;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AMaxPrice then
          AMaxPrice := xCandel.High;
        if xCandel.Low < AMinPrice then
          AMinPrice := xCandel.Low;
      end;
    end;
  end;

  procedure _SetMaxAndMinVol(var AMaxVol, AMinVol: Double);
  var
    i, iCount: Integer;
    xCandel: TCandel;
  begin
    // Предельная значение объема
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AMaxVol := xCandel.Vol;
      AMinVol := xCandel.Vol;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.Vol > AMaxVol then
          AMaxVol := xCandel.Vol;
        if xCandel.Vol < AMinVol then
          AMinVol := xCandel.Vol;
      end;
    end;
  end;

  function _GetPrice(APrice, AMaxPrice, AMinPrice: Double): Double;
  begin
    if (AMaxPrice > 0) and (AMinPrice > 0) then
      Result := (APrice - AMinPrice)/(AMaxPrice - AMinPrice)
    else
      Result := 0;
  end;

  function _GetVol(AVol, AMaxVol, AMinVol: Double): Double;
  begin
    if (AMaxVol > 0) and (AMinVol > 0) then
      Result := (AVol - AMinVol)/(AMaxVol - AMinVol)
    else
      Result := 0;
  end;

var
  xUneCandel, xCandel: TCandel;
  xMaxPrice, xMinPrice: Double;
  xMaxVol, xMinVol: Double;
begin
  FUneCandels.Clear;

  _SetMaxAndMinPrice(xMaxPrice, xMinPrice);
  _SetMaxAndMinVol(xMaxVol, xMinVol);

  for xCandel in ACandels do
  begin
    xUneCandel.Open  := _GetPrice(xCandel.Open, xMaxPrice, xMinPrice);
    xUneCandel.High  := _GetPrice(xCandel.High, xMaxPrice, xMinPrice);
    xUneCandel.Low   := _GetPrice(xCandel.Low,  xMaxPrice, xMinPrice);
    xUneCandel.Close := _GetPrice(xCandel.Close,xMaxPrice, xMinPrice);
    xUneCandel.Vol   := _GetVol(xCandel.Vol, xMaxVol, xMinVol);
    FUneCandels.Add(xUneCandel);
  end;
end;

end.
