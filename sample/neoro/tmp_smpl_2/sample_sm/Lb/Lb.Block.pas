unit Lb.Block;

interface

{$IFDEF DEBUG}
//  {$DEFINE DBL}
{$ENDIF}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  TBlock = class;
  TBlockList = TObjectList<TBlock>;

  ///<summary>Определяем состояние блока</summary>
  TTypeDecision = (tdWait, tdBuy, tdSell);

  ///<summary>Базовый объект</summary>
  TBlock = class(TObject)
  public const
    BLOCK_SIZE = 100;
  private
    FCandels: TCandels;
    function GetCandelLast: TCandel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFileName(const AFileName: String);
    procedure SetParserBlock(const ASources: TStrings);
    property Candels: TCandels read FCandels;
    ///<summary>Последние свеча в блоке</summary>
    property CandelLast: TCandel read GetCandelLast;
  end;

  TBlockAPI = record
    ///<summary>Придельная значение блока</summary>
    class procedure MaxAndMinValueBlock(ABlock: TBlock; var AMaxValue, AMinValue: Double); static;
    ///<summary>Rate of Change - значение блока индикатора</summary>
    class function RcC(ABlock: TBlock): Double; static;
    ///<summary>Предельное значение за период</summary>
    class procedure LimitValueMaxAndMinBlock(ABlock: TBlock; const APeriod: Integer; var AMaxValue, AMinValue: Double); static;
  end;


implementation

uses
{$IFDEF DBL}
  Lb.Logger,
{$ENDIF}
  System.Threading;

{ TBlock }

constructor TBlock.Create;
begin
  FCandels := TCandels.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

function TBlock.GetCandelLast: TCandel;
var
  xIndex: Integer;
begin
  xIndex := FCandels.Count - 1;
  Result := FCandels.Items[xIndex];
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
  FCandels.Clear;
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
end;

{ TBlockAPI }

class procedure TBlockAPI.MaxAndMinValueBlock(ABlock: TBlock; var AMaxValue, AMinValue: Double);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  AMaxValue := 0;
  AMinValue := 0;
  iCount := ABlock.Candels.Count;
  if iCount > 0 then
  begin
    xCandel := ABlock.Candels[0];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := 0 to iCount - 1 do
    begin
      if AMaxValue < xCandel.High then
        AMaxValue := xCandel.High;
      if AMinValue > xCandel.Low then
        AMinValue := xCandel.Low;
    end;
  end;
end;

class function TBlockAPI.RcC(ABlock: TBlock): Double;
var
  xC1, xC2: TCandel;
  iCount: Integer;
begin
  Result := 0.5;
  iCount := ABlock.Candels.Count;
  if iCount > 0 then
  begin
    xC1 := ABlock.Candels[0];
    xC2 := ABlock.Candels[iCount - 1];
    Result := (xC2.Close/xC1.Close) - 0.5;
  end;
end;

class procedure TBlockAPI.LimitValueMaxAndMinBlock(ABlock: TBlock;
  const APeriod: Integer; var AMaxValue, AMinValue: Double);
var
  xCandel: TCandel;
  i, iCount, xInd: Integer;
begin
  AMaxValue := 0;
  AMinValue := 0;
  iCount := ABlock.Candels.Count;
  if iCount > 0 then
  begin
    xInd := iCount - APeriod - 1;
    xCandel := ABlock.Candels[xInd];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;
    for i := xInd to iCount - 2 do
    begin
      xCandel := ABlock.Candels[i];
      if AMaxValue < xCandel.High then
        AMaxValue := xCandel.High;
      if AMinValue > xCandel.Low then
        AMinValue := xCandel.Low;
    end;
  end;
end;

end.
