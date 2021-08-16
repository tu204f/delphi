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
  ///<summary>Ѕлок Ц который отвечает за данные дл€ вектора</summary>
  TBlock = class(TObject)
  private
    FSources: TSourceCandel;
    FVectors: TSourceCandel;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>‘ормирование вектора</summary>
    procedure SetFormationVector;
    ///<summary>‘ормирование источника данных, из ветора и цены</summary>
    procedure SetFormationSource(AIndex: Integer; AMaxPrice, AMinPrice, APrice: Double; ATypePrice: TTypePrice);
    ///<summary>»сточник данных дл€ блока</summary>
    property Sources: TSourceCandel read FSources;
    ///<summary>—оздаем, вектор</summary>
    property Vectors: TSourceCandel read FVectors;
  end;

implementation

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
      Result := (AValue - AMinValue)/(AMaxValue - AMinValue);
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
      xVector.Date  := xCandel.Date;
      xVector.Time  := xCandel.Time;
      xVector.Open  := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Open);
      xVector.High  := GetProcentVector(xMaxPrice,xMinPrice,xCandel.High);
      xVector.Low   := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Low);
      xVector.Close := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Close);
      xVector.Vol   := GetProcentVector(xMaxVol,xMinVol,xCandel.Vol);
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
