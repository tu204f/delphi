unit Lb.Black.Box;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  Lb.Vecrot.Candel,
  Lb.Candel.SysUtils,
  Lb.Candel.Source;

type
  ///<summary>Вектор из себя представляем массив свячей в вектороной форме</summary>
  TVector = class(TObject)
  private
    FValues: TCandelList; // Опорный вектор
    FComing: TCandelList; // Графдущий ветор
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Значение вектора – по которому производим поиск</summary>
    property Values: TCandelList read FValues write FValues;
    ///<summary>Грядущие значение</summary>
    property Coming: TCandelList read FComing write FComing;
  end;

  ///<summary>Список векторов</summary>
  TVectorList = TObjectList<TVector>;

  ///<summary>Список векторов, векторный образ</summary>
  {todo: Сохрание векторного образа}
  TVectors = class(TVectorList)
  private
    FVectorCount: Integer; // Размер опорного вектора
    FComingCount: Integer; // Размер грядушено ветора
  public
    constructor Create;
    procedure SetLoadCandels(const ACandels: TCandelList); overload;
    procedure SetLoadCandels(const AFileName: String); overload;
    ///<summary>Размер одного массива ветора</summary>
    property VectorCount: Integer read FVectorCount write FVectorCount;
    ///<summary>Размер грядушего вектора</summary>
    property ComingCount: Integer read FComingCount write FComingCount;
  end;

  ///<summary>Структура номера индекса</summary>
  TComing = record
    Index: Integer;      // Номер или индекса массива
    LengthPrice: Double; // Длина вектора по цене
    LenghtValue: Double; // Длина вектора по объекму
  public
    function ToString: String;
  end;

  TComingList = TList<TComing>;

  ///<summary>Используемый тип данных</summary>
  TTypeLenght = (tpPrice, tpValue);

  ///<summary>Черный ящик</summary>
  ///<remarks>
  /// Харнит количество совпадений ветора, и у казаному совпадение возарвщает
  /// Массив состояний из двух частей (векторной, и пердсказуемой - ожидание)
  ///</remarks>
  TBlackBox = class(TObject)
  private
    FComings: TComingList;
    FVectors: TVectors;
    FSearchCandels: TCandelList;
    FOnBeginComing: TNotifyEvent;
    FOnEndComing: TNotifyEvent;
  protected {работа с потоком, все данные отрабатываются отдельно}
    FTask: ITask;
    procedure StartComingTask;
    procedure DoBeginComing;   // Начало поиска
    procedure DoEndComing;     // Конец поиска
    procedure DoInsertComing(const AComing: TComing);
  protected
    procedure InsertComing(const AIndex: Integer; const ALengthPrice, ALenghtValue: Double);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SearchCandel(const ACandels: TCandelList);
    procedure ComingCandel(const AIndex: Integer; const ACandels: TCandelList);
    ///<summary>Список векторов</summary>
    property Vectors: TVectors read FVectors write FVectors;
    ///<summary>Ожидание списко</summary>
    property Comings: TComingList read FComings write FComings;
  public
    property OnBeginComing: TNotifyEvent write FOnBeginComing;
    property OnEndComing: TNotifyEvent write FOnEndComing;
  end;

implementation

uses
  System.Math;

type
  TTypeCandelValue = (tcvOpen, tcvHigh, tcvLow, tcvClose, tcvValue);
  TTypeCandelValues = set of TTypeCandelValue;

// Форматирование в векторное представление
procedure ConvertFormatVactorToCandel(const ACandels, AVectorCandels: TCandelList; const APointPrice: Double = 0);
var
  i, iCount: Integer;
  xPointPrice: Double;
  xCandel, xResult: TCandel;
begin
  AVectorCandels.Clear;
  iCount := ACandels.Count;
  if iCount > 0 then
  begin
    // Текущая свеча - и цена закрытия является базовой
    if APointPrice = 0 then
      xPointPrice := ACandels[iCount - 1].Close
    else
      xPointPrice := APointPrice;
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      xResult.Date := xCandel.Date;
      xResult.Time := xCandel.Time;
      xResult.Open  := xCandel.Open /xPointPrice;
      xResult.High  := xCandel.High /xPointPrice;
      xResult.Low   := xCandel.Low  /xPointPrice;
      xResult.Close := xCandel.Close/xPointPrice;
      xResult.Vol   := xCandel.Vol  /xPointPrice;
      AVectorCandels.Add(xResult);
    end;
  end;
end;

// Разница межуд векторами
procedure DifferenceVector(const AVectorA, AVectorB, AVectorC: TCandelList);
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

// Подсчитываем длину вектора
procedure LengthVector(const AVector: TCandelList; var ALengthPrice, ALenghtValue: Double; ATypeCandelValues: TTypeCandelValues = []);
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

{ TVector }

constructor TVector.Create;
begin
  FValues := TCandelList.Create;
  FComing := TCandelList.Create;
end;

destructor TVector.Destroy;
begin
  FreeAndNil(FComing);
  FreeAndNil(FValues);
  inherited;
end;

{ TVectors }

constructor TVectors.Create;
begin
  inherited;
  FVectorCount := 20;
end;

procedure TVectors.SetLoadCandels(const ACandels: TCandelList);

  procedure _ConvertToVector(const ACandels, AComing: TCandelList; AVector: TVector);
  var
    iCount: Integer;
  begin
    AVector.Values.Clear;
    AVector.Coming.Clear;
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      var xCurrent := ACandels[iCount - 1];
      ConvertFormatVactorToCandel(ACandels,AVector.Values,xCurrent.Close);
      iCount := AComing.Count;
      if iCount > 0 then
        ConvertFormatVactorToCandel(AComing,AVector.Coming,xCurrent.Close);
    end;
  end;

var
  xIndex: Integer;

  function _Eof(ACandels: TCandelList): Boolean;
  begin
    Result := (xIndex + FVectorCount + FComingCount) >= ACandels.Count;
  end;

begin
  if FVectorCount <= 0 then
    raise Exception.Create('Error Message: Размер вектора не задан');
  xIndex := 0;
  while not _Eof(ACandels) do
  begin
    var xCandels := TCandelList.Create; // Опорное занчение
    var xComing  := TCandelList.Create; // Для определение градущего ветора
    var xVector  := TVector.Create;
    try
      // Делаем копию массива свечай
      for var i := 0 to FVectorCount - 1 do
      begin
        var Ind := xIndex + i;
        var xCandel := ACandels[Ind];
        xCandels.Add(xCandel);
      end;
      // Создаем грядущие значение
      for var i := 0 to FComingCount - 1 do
      begin
        var Ind := (xIndex + FVectorCount) + i;
        var xCandel := ACandels[Ind];
        xComing.Add(xCandel);
      end;
      // Формитируем все данные в векторный формат
      _ConvertToVector(xCandels,xComing,xVector);
      Self.Add(xVector);
      Inc(xIndex);
    finally
      FreeAndNil(xComing);
      FreeAndNil(xCandels);
    end;
  end;
end;

procedure TVectors.SetLoadCandels(const AFileName: String);
var
  xSourceCandel: TSourceCandel;
begin
  xSourceCandel := TSourceCandel.Create;
  try
    xSourceCandel.SetLoadFile(AFileName);
    SetLoadCandels(xSourceCandel.Candels);
  finally
    FreeAndNil(xSourceCandel);
  end;
end;

{ TComing }

function TComing.ToString: String;
begin
  // Пораметры скрипта
  Result := 'Вектор: ' + Self.Index.ToString + '; ' +
                         Self.LengthPrice.ToString + '; ' +
                         Self.LenghtValue.ToString;
end;

{ TBlackBox }

constructor TBlackBox.Create;
begin
  FVectors := TVectors.Create;
  FComings := TComingList.Create;
  FSearchCandels := nil;
end;

destructor TBlackBox.Destroy;
begin
  if Assigned(FTask) then
  begin
    FTask.Cancel;
    FTask := nil;
  end;
  FreeAndNil(FComings);
  FreeAndNil(FVectors);
  inherited;
end;

procedure TBlackBox.InsertComing(const AIndex: Integer; const ALengthPrice, ALenghtValue: Double);
{$DEFINE LENGTH_PRICE}

var
  xNotValue: Boolean;
  xComing: TComing;
  i, iCount: Integer;
begin
  xNotValue := True;
  iCount := FComings.Count;
  if iCount > 0 then
    for i := 0 to  iCount - 1 do
      {$IFDEF LENGTH_PRICE}
      if ALengthPrice <= FComings[i].LengthPrice then
      {$ELSE}
      if ALenghtValue <= FComings[i].LenghtValue then
      {$ENDIF}
      begin
        xComing.Index := AIndex;
        xComing.LengthPrice := ALengthPrice;
        xComing.LenghtValue := ALenghtValue;
        FComings.Insert(i,xComing);
        xNotValue := False;
        Break;
      end;

  if xNotValue then
  begin
    xComing.Index := AIndex;
    xComing.LengthPrice := ALengthPrice;
    xComing.LenghtValue := ALenghtValue;
    FComings.Add(xComing);
  end;

  DoInsertComing(xComing);

  if FComings.Count > 50 then
    FComings.Delete(FComings.Count - 1);
end;

procedure TBlackBox.SearchCandel(const ACandels: TCandelList);
begin
  FSearchCandels := ACandels;
  StartComingTask;
end;

procedure TBlackBox.StartComingTask;
begin
  // почему поток не работает
  FTask := TTask.Create(
    procedure()
    var
      xCandelFormateVector: TCandelList;
      i, iCount: Integer;
      xLengthPrice, xLenghtValue: Double;
    begin
      TThread.Queue(nil,
        DoBeginComing
      );
      FComings.Clear;
      xCandelFormateVector := TCandelList.Create;
      try
        ConvertFormatVactorToCandel(FSearchCandels,xCandelFormateVector);
        iCount := FVectors.Count;
        if iCount > 0 then
          for i := iCount - 1 downto 0 do
          begin
            FTask.CheckCanceled;
            var xValueC := TCandelList.Create;
            try
              DifferenceVector(FVectors[i].Values,xCandelFormateVector,xValueC);
              LengthVector(xValueC,xLengthPrice,xLenghtValue);
              InsertComing(i,xLengthPrice,xLenghtValue);
            finally
              FreeAndNil(xValueC);
            end;
          end;
      finally
        FreeAndNil(xCandelFormateVector);
      end;

      TThread.Queue(nil,
        DoEndComing
      );
    end
  );
  FTask.Start;
end;

procedure TBlackBox.DoBeginComing;
begin
  {Пока посласть сообщение начали поик подходящего вектора}
  if Assigned(FOnBeginComing) then FOnBeginComing(Self);
end;

procedure TBlackBox.DoEndComing;
begin
  {Нашли оптимальные вектора, которые наиболее чтоно описывают состояние}
  if Assigned(FOnEndComing) then FOnEndComing(Self);
end;

procedure TBlackBox.DoInsertComing(const AComing: TComing);
begin

end;

procedure TBlackBox.ComingCandel(const AIndex: Integer; const ACandels: TCandelList);
var
  xCandel: TCandel;
begin
  if not Assigned(FSearchCandels) then
    Exit;

  if (AIndex >= 0) and (AIndex < FComings.Count) then
  begin
    var xInd := FComings[AIndex].Index;
    var xComing := FVectors[xInd].Coming;

    ACandels.Clear;
    var xLastCandel := FSearchCandels[FSearchCandels.Count - 1];
    for var xVectorCandel in xComing do
    begin
      xCandel.Date  := 0;
      xCandel.Time  := 0;
      xCandel.Open  := xVectorCandel.Open  * xLastCandel.Close;
      xCandel.High  := xVectorCandel.High  * xLastCandel.Close;
      xCandel.Low   := xVectorCandel.Low   * xLastCandel.Close;
      xCandel.Close := xVectorCandel.Close * xLastCandel.Close;
      xCandel.Vol   := xVectorCandel.Vol   * xLastCandel.Vol;
      ACandels.Add(xCandel);
    end;
  end
  else
    raise Exception.Create('Error Message: Ожидание не созданны');
end;



end.
