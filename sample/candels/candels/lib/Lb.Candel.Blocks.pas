unit Lb.Candel.Blocks;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils,
  Lb.Candel.Source;

type
  ///<summary>Блок – который отвечает за данные для вектора</summary>
  TBlock = class(TObject)
  private
    FSources: TSourceCandel;
    FVectors: TSourceCandel;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>Формирование вектора</summary>
    procedure SetFormationVector;
    ///<summary>Формирование источника данных, из ветора и цены</summary>
    procedure SetFormationSource(AIndex: Integer; AMaxPrice, AMinPrice, APrice: Double; ATypePrice: TTypePrice);
    ///<summary>Источник данных для блока</summary>
    property Sources: TSourceCandel read FSources;
    ///<summary>Создаем, вектор</summary>
    property Vectors: TSourceCandel read FVectors;
  end;

///<summary>Сравнить два блока</summary>
///<remarks>
/// Сравнение двух блоков:
///  0   - польное сравнение
///  100 - разные блоки
///</remarks>
function GetSameBlock(const ABlock1, ABlock2: TBlock; ATypePrice: TTypePrice = TTypePrice.tpClose): Boolean;

implementation

///<summary>Протокольная функция</summary>
///<remarks>Cравнении определённой точностью, если откланение DELTA_SAM</remarks>
function GetProtocolFunction(const AValue1, AValue2: Double): Boolean;
const
  DELTA_SAME = 10;
var
  xDelta: Double;
begin
  xDelta := AValue1 - AValue2;
  if xDelta < 0 then
    xDelta := -1 * xDelta;
  Result := xDelta < DELTA_SAME;
end;

function GetSameBlock(const ABlock1, ABlock2: TBlock; ATypePrice: TTypePrice): Boolean;
var
  xResult: Boolean;
  xV1, xV2: TCandel;
  i, iCount: Integer;
begin
  Result := False;
  {todo: Нужно вести понятие протокольная функция}
  if not Assigned(ABlock1) then
    raise Exception.Create('Error Message: Блок 1 не определен');
  if not Assigned(ABlock2) then
    raise Exception.Create('Error Message: Блок 2 не определен');

  iCount := ABlock1.Vectors.Candels.Count;
  if ABlock2.Vectors.Candels.Count <> iCount then
    raise Exception.CreateFmt('Error Message: Блок1.Count = %d Блок2.Count = %d',
      [ABlock1.Vectors.Candels.Count, ABlock2.Vectors.Candels.Count]);

  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xV1 := ABlock1.Vectors.Candels[i];
      xV2 := ABlock2.Vectors.Candels[i];

      if xV1.Status in [2,3] then
        Continue;
      case ATypePrice of
        tpNon: begin
          xResult :=
            GetProtocolFunction(xV1.Open ,xV2.Open) and
            GetProtocolFunction(xV1.High ,xV2.High) and
            GetProtocolFunction(xV1.Low  ,xV2.Low) and
            GetProtocolFunction(xV1.Close,xV2.Close);
        end;
        tpOpen: begin
          xResult :=
            GetProtocolFunction(xV1.Open ,xV2.Open);
        end;
        tpHigh: begin
          xResult :=
            GetProtocolFunction(xV1.High ,xV2.High);
        end;
        tpLow: begin
          xResult :=
            GetProtocolFunction(xV1.Low  ,xV2.Low);
        end;
        tpClose: begin
          xResult :=
            GetProtocolFunction(xV1.Close,xV2.Close);
        end;
      end;

      if not xResult then
        Break;
    end;
  Result := xResult;
end;

{ TBlock }

constructor TBlock.Create;
begin
  FSources := TSourceCandel.Create;
  FVectors := TSourceCandel.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FVectors);
  FreeAndNil(FSources);
  inherited;
end;

procedure TBlock.SetFormationVector;

  function GetProcentVector(const AMaxValue, AMinValue, AValue: Double): Double;
  begin
    Result := 0;
    if (AMaxValue > 0) and (AMinValue > 0) and (AValue > 0) then
    begin
      var xDelta := (AValue - AMinValue)/(AMaxValue - AMinValue);
      Result := Trunc(xDelta * 100);
    end;
  end;

var
  xCandel: TCandel;
  xVector: TCandel;
  xMaxPrice, xMinPrice: Double;
  xMaxVol, xMinVol: Double;
begin
  FVectors.Candels.Clear;
  if FSources.Candels.Count > 0 then
  begin
    FSources.GetMaxAndMinPriceVol(xMaxPrice,xMinPrice,xMaxVol,xMinVol);
    for xCandel in FSources.Candels do
    begin
      xVector.Date   := xCandel.Date;
      xVector.Time   := xCandel.Time;
      xVector.Open   := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Open);
      xVector.High   := GetProcentVector(xMaxPrice,xMinPrice,xCandel.High);
      xVector.Low    := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Low);
      xVector.Close  := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Close);
      xVector.Vol    := GetProcentVector(xMaxVol,xMinVol,xCandel.Vol);
      xVector.Status := xCandel.Status;
      FVectors.Candels.Add(xVector);
    end;
  end;
end;


procedure TBlock.SetFormationSource(AIndex: Integer; AMaxPrice, AMinPrice, APrice: Double; ATypePrice: TTypePrice);

  function GetToProventCandel(const AValue: Double): Double;
  begin
    Result := (AMaxPrice - AMinPrice) * AValue + AMinPrice;
  end;

var
  xCandel: TCandel;
  xVector: TCandel;
  iCount: Integer;
  xPrice: Double;
begin
  FSources.Candels.Clear;
  iCount := FVectors.Candels.Count;
  if (AIndex >= 0) and (AIndex < iCount) then
  begin
    xVector := FVectors.Candels[AIndex];
    xPrice := xVector.Close;
    for xVector in FVectors.Candels do
    begin
      xCandel.Date  := xVector.Date;
      xCandel.Time  := xVector.Time;
      xCandel.Open  := GetToProventCandel(xVector.Open);
      xCandel.High  := GetToProventCandel(xVector.High);
      xCandel.Low   := GetToProventCandel(xVector.Low);
      xCandel.Close := GetToProventCandel(xVector.Close);
      xCandel.Vol   := GetToProventCandel(xVector.Vol);
      FSources.Candels.Add(xVector);
    end;

  end;
end;


end.
