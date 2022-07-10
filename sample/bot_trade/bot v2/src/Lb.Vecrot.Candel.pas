unit Lb.Vecrot.Candel;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Candel.SysUtils,
  Lb.Candel.Source;

type
  TVecrotSystem = record
  public type
    TTypeCandelValue = (tcvOpen, tcvHigh, tcvLow, tcvClose, tcvValue);
    TTypeCandelValues = set of TTypeCandelValue;
  public
    ///<summary>Конвертируем формат</summary>
    class procedure ConvertFormatVactorToCandel(const ACandels, AVectorCandels: TCandelList); static;
    ///<summary>Разница между векторами</summary>
    class procedure DifferenceVector(const AVectorA, AVectorB, AVectorC: TCandelList); static;
    ///<summary>Вычисляем длину вектора</summary>
    class procedure LengthVector(const AVector: TCandelList; var ALengthPrice, ALenghtValue: Double; ATypeCandelValues: TVecrotSystem.TTypeCandelValues = []); static;
  end;

  ///<summary>Объект управленеи потоком</summary>
  ///<remarks>Производим поиск ближайшего вектора, реализация черного ящика</remarks>
  TManagerThread = class(TObject)
  public type
    TTypeStatus = (tsBegin,tsEnd);

    ///<summary>Массив длин векторов</summary>
    TLengthVector = record
      LengthPrice: Double;   // Длина вектора по цене
      LenghtValue: Double;   // Длина вектора по объему
      IndexCandel: Integer;  // Номер индекса свячи
    end;
    TLengthVectorList = TList<TLengthVector>;

  private
    FLengthVectors: TLengthVectorList;
    FActive: Boolean;
    FFileName: String;
    FSearchCandels: TCandelList;
    FSourceCandel: TSourceCandel;
    procedure SetFileName(const Value: String);
    function GetSearchCandels: TCandelList;
    procedure InsertLengthResultVector(const ALengthPrice, ALenghtValue: Double; const AIndexCandel: Integer);
    procedure SetSearchCandels(const Value: TCandelList);
  protected
    ///<summary>Процедура исполнение потока</summary>
    procedure ExecutionThread;
    ///<summary>Источник свечай</summary>
    property SourceCandel: TSourceCandel read FSourceCandel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    ///<summary>Путь к файлу истории</summary>
    property FileName: String read FFileName write SetFileName;
    property Active: Boolean read FActive;
    property SearchCandels: TCandelList read GetSearchCandels write SetSearchCandels;
    property LengthVectors: TLengthVectorList read FLengthVectors;
  end;

implementation

uses
  System.Threading,
  System.Math;

class procedure TVecrotSystem.ConvertFormatVactorToCandel(const ACandels, AVectorCandels: TCandelList);
var
  i, iCount: Integer;
  xCurrentCandel, xCandel, xResult: TCandel;
begin
  AVectorCandels.Clear;
  iCount := ACandels.Count;
  if iCount > 0 then
  begin
    // Текущая свеча - и цена закрытия является базовой
    xCurrentCandel := ACandels[iCount - 1];
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      xResult.Date := xCandel.Date;
      xResult.Time := xCandel.Time;
      xResult.Open  := xCandel.Open /xCurrentCandel.Close;
      xResult.High  := xCandel.High /xCurrentCandel.Close;
      xResult.Low   := xCandel.Low  /xCurrentCandel.Close;
      xResult.Close := xCandel.Close/xCurrentCandel.Close;
      xResult.Vol   := xCandel.Vol  /xCurrentCandel.Vol;
      AVectorCandels.Add(xResult);
    end;
  end;
end;

class procedure TVecrotSystem.DifferenceVector(const AVectorA, AVectorB, AVectorC: TCandelList);
var
  i, iCount: Integer;
  xCandel1, xCandel2, xCandelResult: TCandel;
begin
  AVectorC.Clear;
  if (AVectorA.Count = AVectorB.Count) and (AVectorA.Count > 0) then
  begin
    iCount := AVectorA.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel1 := AVectorA[i];
        xCandel2 := AVectorB[i];
        FillChar(xCandelResult,SizeOf(xCandelResult),0);
        xCandelResult.Open  := xCandel1.Open  - xCandel2.Open;
        xCandelResult.High  := xCandel1.High  - xCandel2.High;
        xCandelResult.Low   := xCandel1.Low   - xCandel2.Low;
        xCandelResult.Close := xCandel1.Close - xCandel2.Close;
        xCandelResult.Vol   := xCandel1.Low   - xCandel2.Low;
        AVectorC.Add(xCandelResult);
      end;
  end;
end;

class procedure TVecrotSystem.LengthVector(const AVector: TCandelList; var ALengthPrice, ALenghtValue: Double; ATypeCandelValues: TTypeCandelValues = []);
var
  xSumPrice, xSumValue: Double;
  i, iCount: Integer;
  xCandel: TCandel;
begin
  xSumPrice := 0;
  xSumValue := 0;
  iCount := AVector.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := AVector[i];
      if ATypeCandelValues = [] then
      begin
        xSumPrice := xSumPrice + Power(xCandel.Open,2) + Power(xCandel.High,2) + Power(xCandel.Low,2) + Power(xCandel.Close,2);
        xSumValue := xSumValue + Power(xCandel.Vol,2);
      end
      else
      begin
        if tcvOpen in ATypeCandelValues then
          xSumPrice := xSumPrice + Power(xCandel.Open,2);
        if tcvHigh in ATypeCandelValues then
          xSumPrice := xSumPrice + Power(xCandel.High,2);
        if tcvLow in ATypeCandelValues then
          xSumPrice := xSumPrice + Power(xCandel.Low,2);
        if tcvClose in ATypeCandelValues then
          xSumPrice := xSumPrice + Power(xCandel.Close,2);
        if tcvValue in ATypeCandelValues then
          xSumValue := xSumValue + Power(xCandel.Vol,2);
      end;
    end;
  ALengthPrice := Power(xSumPrice,0.5);
  ALenghtValue := Power(xSumValue,0.5);
end;

{ TManagerThread }

constructor TManagerThread.Create;
begin
  FActive := False;
  FSourceCandel := TSourceCandel.Create;
  FSearchCandels := nil;
  FLengthVectors := TLengthVectorList.Create;
end;

destructor TManagerThread.Destroy;
begin
  FreeAndNil(FLengthVectors);
  FreeAndNil(FSourceCandel);
  inherited;
end;

procedure TManagerThread.InsertLengthResultVector(const ALengthPrice,
  ALenghtValue: Double; const AIndexCandel: Integer);
var
  xNotValue: Boolean;
  i, iCount: Integer;
  xLengthResultVector, xL: TLengthVector;
begin
  xNotValue := True;
  iCount := FLengthVectors.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xLengthResultVector := FLengthVectors[i];
      if (xLengthResultVector.LengthPrice >= ALengthPrice) then
      begin
        xL.LengthPrice := ALengthPrice;
        xL.LenghtValue := ALenghtValue;
        xL.IndexCandel := AIndexCandel;
        FLengthVectors.Insert(i,xL);
        xNotValue := False;
        Break;
      end;
    end;
  if xNotValue then
  begin
    xL.LengthPrice := ALengthPrice;
    xL.LenghtValue := ALenghtValue;
    xL.IndexCandel := AIndexCandel;
    FLengthVectors.Add(xL);
  end;
end;

procedure TManagerThread.ExecutionThread;
var
  xIndexSourceCandelInc: Integer;

  procedure _NextBlock(ACount: Integer; ABlock: TCandelList);
  begin
    if not Assigned(ABlock) then
      Exit;
    ABlock.Clear;
    for var i := 0 to ACount - 1 do
    begin
      var xInd := xIndexSourceCandelInc + i;
      if xInd >= SourceCandel.Candels.Count then
      begin
        ABlock.Clear;
        Self.Stop;
        Break;
      end;
      var xCandel := SourceCandel.Candels[xInd];
      ABlock.Add(xCandel);
    end;
    Inc(xIndexSourceCandelInc);
  end;

  procedure _Execution;
  var
    xCandels, xVectors, xSearchVectors, xVectorC: TCandelList;
    xLengthPrice, xLenghtValue: Double;
  begin
    xCandels := TCandelList.Create;
    xVectors := TCandelList.Create;
    xVectorC := TCandelList.Create;
    xSearchVectors := TCandelList.Create;
    try
      _NextBlock(FSearchCandels.Count, xCandels);
      TVecrotSystem.ConvertFormatVactorToCandel(xCandels,xVectors);
      TVecrotSystem.ConvertFormatVactorToCandel(FSearchCandels,xSearchVectors);
      TVecrotSystem.DifferenceVector(xVectors, xSearchVectors, xVectorC);
      TVecrotSystem.LengthVector(xVectorC, xLengthPrice, xLenghtValue,[]);
      InsertLengthResultVector(xLengthPrice,xLenghtValue,xIndexSourceCandelInc + FSearchCandels.Count);
    finally
      FreeAndNil(xSearchVectors);
      FreeAndNil(xVectorC);
      FreeAndNil(xVectors);
      FreeAndNil(xCandels);
    end;
  end;

begin
  if not Assigned(FSearchCandels) then
    Exit;
  if FSearchCandels.Count <= 0 then
    Exit;

  FSourceCandel.SetLoadFile(FFileName);
  xIndexSourceCandelInc := 0;
  while FActive do
  begin
    _Execution;
    TThread.Sleep(1000);
  end;
end;

procedure TManagerThread.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

function TManagerThread.GetSearchCandels: TCandelList;
begin
  if not Assigned(FSearchCandels) then
    FSearchCandels := TCandelList.Create;
  Result := FSearchCandels;
end;

procedure TManagerThread.SetSearchCandels(const Value: TCandelList);
begin
  FSearchCandels := Value;
end;

procedure TManagerThread.Start;
var
  xTask: ITask;
begin
  if not FActive then
  begin
    FActive := True;
    xTask := TTask.Run(
      ExecutionThread
    );
    xTask := nil;
  end;
end;

procedure TManagerThread.Stop;
begin
  if FActive then
  begin
    FActive := False;
    // Остановить испольнение потока
  end;
end;

end.
