unit Lb.ABot;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Candel.SysUtils;

type
  ///<summary>Уровень роста - для оценки динамики</summary>
  TLevelList = TList<Double>;

  ///<summary>Ячейка патерна поведение</summary>
  ///<remraks>Единичная ячека поведение, на основание которой можно судить, что будет дальше</remerks>
  TVectorCell = class(TObject)
  private
    FID: Integer;
    FSources: TCandelList;
    FLevels: TLevelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(AVectorCell: TVectorCell);
    procedure AssignValue(AVectorCell: TVectorCell);
    ///<summary>Память ячейки</summary>
    ///<remarks>Здесь хроняться все</remarks>
    property Sources: TCandelList read FSources;
    ///<summary>Ожидание ячейки</sumary>
    property Levels: TLevelList read FLevels;
    ///<summary>Номер ячеки</summary>
    property ID: Integer read FID write FID;
  end;

  ///<summary>Список ячейке</summary>
  TVectorCellList = TObjectList<TVectorCell>;

  ///<summary>Объект сравнения, основного вектора</summary>
  TVectorCellСompare = class(TVectorCell)
  private
    FCompare: TVectorCell;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Compare(const ACompare: TVectorCell);
  end;


implementation

uses
  System.Math;

///<summary>Процедура преобразует значение в относительные значение</summary>
procedure PriceToDelta(const AVectorCell: TVectorCell);
var
  xBotCell: TVectorCell;
  i, iCount: Integer;
  j, jCount: Integer;
  xLevelValue: Double;
  xCurrentCandel, xCandel, xResult: TCandel;
begin
  xBotCell := TVectorCell.Create;
  try
    iCount := AVectorCell.Sources.Count;
    if iCount > 0 then
    begin
      // Текущая свеча - и цена закрытия является базовой
      xCurrentCandel := AVectorCell.Sources[iCount - 1];
      for i := 0 to iCount - 1 do
      begin
        xCandel := AVectorCell.Sources[i];
        xResult.Open  := xCandel.Open /xCurrentCandel.Close;
        xResult.High  := xCandel.High /xCurrentCandel.Close;
        xResult.Low   := xCandel.Low  /xCurrentCandel.Close;
        xResult.Close := xCandel.Close/xCurrentCandel.Close;
        xResult.Vol   := xCandel.Vol  /xCurrentCandel.Vol;
        xBotCell.Sources.Add(xResult);
      end;
      // Все тоже самое, для показателей уровней
      jCount := AVectorCell.Levels.Count;
      if jCount > 0 then
        for j := 0 to jCount - 1 do
        begin
          xLevelValue := AVectorCell.Levels[j]/xCurrentCandel.Close;
          xBotCell.Levels.Add(xLevelValue);
        end;
    end;
    AVectorCell.AssignValue(xBotCell);
  finally
    FreeAndNil(xBotCell);
  end;
end;

///<summary>Вычисляем длину вектора</summary>
procedure LengthVector(const AVectorCell: TVectorCell; var ALengthPrice, ALenghtValue: Double);
var
  xSumPrice, xSumValue: Double;
  i, iCount: Integer;
  xCandel: TCandel;
begin
  xSumPrice := 0;
  xSumValue := 0;
  iCount := AVectorCell.Sources.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := AVectorCell.Sources[i];
      xSumPrice := xSumPrice + Power(xCandel.Open,2) + Power(xCandel.High,2) + Power(xCandel.Low,2) + Power(xCandel.Close,2);
      xSumValue := xSumValue + Power(xCandel.Vol,2);
    end;
  ALengthPrice := Power(xSumPrice,0.5);
  ALenghtValue := Power(xSumValue,0.5);
end;

///<summary>Вычисляем разницу</summary>
procedure DifferenceVector(const AVector1, AVector2, AVectorResult: TVectorCell);
var
  i, iCount: Integer;
  xCandel1, xCandel2, xCandelResult: TCandel;
begin
  AVectorResult.Sources.Clear;
  if AVector1.Sources.Count = AVector2.Sources.Count then
  begin
    iCount := AVector1.Sources.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel1 := AVector1.Sources[i];
        xCandel2 := AVector2.Sources[i];
        FillChar(xCandelResult,SizeOf(xCandelResult),0);
        xCandelResult.Open  := xCandel1.Open  - xCandel2.Open;
        xCandelResult.High  := xCandel1.High  - xCandel2.High;
        xCandelResult.Low   := xCandel1.Low   - xCandel2.Low;
        xCandelResult.Close := xCandel1.Close - xCandel2.Close;
        xCandelResult.Vol   := xCandel1.Low   - xCandel2.Low;
        AVectorResult.Sources.Add(xCandelResult);
      end;
  end;
end;

{ TVectorCell }

constructor TVectorCell.Create;
begin
  FSources := TCandelList.Create;
  FLevels  := TLevelList.Create;
end;

destructor TVectorCell.Destroy;
begin
  FreeAndNil(FLevels);
  FreeAndNil(FSources);
  inherited;
end;

procedure TVectorCell.Assign(AVectorCell: TVectorCell);
begin
  Self.ID := AVectorCell.ID;
  Self.AssignValue(AVectorCell);
end;

procedure TVectorCell.AssignValue(AVectorCell: TVectorCell);
var
  i, iCount: Integer;
  xCandel: TCandel;
  xLevel: Double;
begin
  FSources.Clear;
  FLevels.Clear;

  iCount := AVectorCell.Sources.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := AVectorCell.Sources[i];
      FSources.Add(xCandel);
    end;

  iCount := AVectorCell.Levels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xLevel := AVectorCell.Levels[i];
      FLevels.Add(xLevel);
    end;
end;

{ TVectorCellСompare }

constructor TVectorCellСompare.Create;
begin
  FCompare := TVectorCell.Create;
end;

destructor TVectorCellСompare.Destroy;
begin
  FreeAndNil(FCompare);
  inherited;
end;

procedure TVectorCellСompare.Compare(const ACompare: TVectorCell);
begin
   {Считаем вектор - по отношение к вектору Compare};
   // FCompare := {ACompare - Self}

end;

end.
