unit Lb.Generation;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.Trader,
  Lb.Sort,
  Lb.NeuronNet;

type
  ///<summary>Список генов</summary>
  TGenList = TList<Double>;

  ///<summary>Поколение - трейдеров</summary>
  TGeneration = class(TObject)
  private
    FID: Integer;
    FTraderMans: TTraderManList;
    FInfoTraders: TInfoTraderList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Номер поколения</summary>
    property ID: Integer read FID write FID;
    ///<summary>Поколение трейдеров</summary>
    property TraderMans: TTraderManList read FTraderMans;
    ///<summary>Лучший трейдеры</summary>
    property InfoTraders: TInfoTraderList read FInfoTraders;
    ///<summary>Поколение живое или нет</summary>
    function IsAllKill: Boolean;
    ///<summary>Количество еще актиыных трейдеров</summary>
    function GetCountActiveTrader: Integer;
  public
    ///<summary>Установить размер поколения</summary>
    procedure SetCreateTrader(const ACountTrader: Integer);
    procedure Calculate(const AIndexBlock: Integer; const ACurrentPrice: Double; const AValues: array of Double);
    procedure SerSortResult;
  end;


  ///<summary>Родитель</summary>
  TParent = class(TObject)
  private
    FGens: TGenList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Gens: TGenList read FGens;
  end;

  ///<summary>Список родителей</summary>
  TParentList = TObjectList<TParent>;


procedure SetParentsToGeneration(const AGeneration: TGeneration; const AParents: TParentList);
procedure SetGenerationToParents(const AParents: TParentList; const ANewGeneration: TGeneration);

procedure SetCrossingParents(const AParents: TParentList);
procedure SetMutationParents(const AParents: TParentList);


implementation

(******************************************************************************)
(* Процедура переноса данных                                                  *)
(******************************************************************************)

procedure SetParentToTradeMan(const ATrader: TTraderMan; const AParent: TParent);

  procedure _WeightsToGens(AWeights: TWeights; AGens: TGenList);
  var
    xV: Double;
    i, iCount: Integer;
    j, jCount: Integer;
  begin
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    for i := 0 to iCount - 1 do
      for j := 0 to jCount - 1 do
      begin
        xV := AWeights.Cells[i,j];
        AGens.Add(xV);
      end;
  end;

var
  xWeights: TWeights;
  i, iCount: Integer;
begin
  AParent.Gens.Clear;
  iCount := ATrader.NeuronNet.WeightsCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xWeights := ATrader.NeuronNet.Weights[i];
      _WeightsToGens(xWeights,AParent.Gens);
    end;
end;


procedure SetTradeManToParent(const AParent: TParent; const ATrader: TTraderMan);

  procedure _GensToWeights(AGens: TGenList; AWeights: TWeights);
  var
    xV: Double;
    xInd: Integer;
    i, iCount: Integer;
    j, jCount: Integer;
  begin
    xInd := 0;
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    for i := 0 to iCount - 1 do
      for j := 0 to jCount - 1 do
      begin
        xV := AGens[xInd];
        AWeights.Cells[i,j] := xV;
        Inc(xInd);
      end;
  end;

var
  xParent: TParent;
  xWeights: TWeights;
  i, iCount: Integer;
begin
  iCount := ATrader.NeuronNet.WeightsCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xWeights := ATrader.NeuronNet.Weights[i];
      _GensToWeights(AParent.Gens,xWeights);
    end;
end;

///<summary>Заполнили список родителей</summary>
procedure SetParentsToGeneration(const AGeneration: TGeneration; const AParents: TParentList);

  function _GetTurnir(AGeneration: TGeneration): TTraderMan;
  var
    xR: TTraderMan;
    xAge: Integer;
    Count: Integer;
    xInd_1, xInd_2, xInd_3: Integer;
    xTrader1, xTrader2, xTrader3: TTraderMan;
  begin
    // Из троих выбираем лучьшиии
    Count := AGeneration.TraderMans.Count;
    xInd_1 := Random(Count);
    xInd_2 := Random(Count);
    xInd_3 := Random(Count);

    xTrader1 := AGeneration.TraderMans[xInd_1];
    xTrader2 := AGeneration.TraderMans[xInd_2];
    xTrader3 := AGeneration.TraderMans[xInd_3];


    xAge := xTrader1.Age;
    xR := xTrader1;

    if xTrader2.Age > xAge then
    begin
      xR := xTrader2;
      xAge := xR.Age;
    end;

    if xTrader3.Age > xAge then
    begin
      xR := xTrader3;
      xAge := xR.Age;
    end;

    Result := xR;
  end;

var
  xParent: TParent;
  xTraderMan: TTraderMan;
  xInfoTrader: TInfoTrader;
  i, iCount: Integer;
begin
  // Для начало переносим на и более успешных особей,
  // а остальных выбираем турнирным способом
  iCount := AGeneration.InfoTraders.Count;
  if iCount > 3 then
    iCount := 3;

  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xInfoTrader := AGeneration.InfoTraders[i];
      xTraderMan := AGeneration.TraderMans[xInfoTrader.TraderID];
      xParent := TParent.Create;
      SetParentToTradeMan(xTraderMan,xParent);
      AParents.Add(xParent);
    end;

  // Организуем турнир
  while AParents.Count < AGeneration.TraderMans.Count do
  begin
    xTraderMan := _GetTurnir(AGeneration);
    xParent := TParent.Create;
    SetParentToTradeMan(xTraderMan,xParent);
    AParents.Add(xParent);
  end;
end;

///<summary>Запольняем новое полоколение данными</summary>
procedure SetGenerationToParents(const AParents: TParentList; const ANewGeneration: TGeneration);
var
  xTraderMan: TTraderMan;
  xParent: TParent;
  i, iCount: Integer;
begin
  ANewGeneration.TraderMans.Clear;
  iCount := AParents.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xParent := AParents[i];

      xTraderMan := TTraderMan.Create;
      xTraderMan.ID := i;

      SetTradeManToParent(xParent,xTraderMan);

      ANewGeneration.TraderMans.Add(xTraderMan);
    end;

//  xTraderMan := TTraderMan.Create;
//  xTraderMan.ID := iCount;
//  ANewGeneration.TraderMans.Add(xTraderMan);
//
//  xTraderMan := TTraderMan.Create;
//  xTraderMan.ID := iCount;
//  ANewGeneration.TraderMans.Add(xTraderMan);
//
//
//
//  if ANewGeneration.TraderMans.Count > 100 then
//  begin
//    ANewGeneration.TraderMans.Delete(Random(100));
//    ANewGeneration.TraderMans.Delete(Random(100));
//  end;
  
end;

(******************************************************************************)
(* Реализация алгоритма перемешивание генов                                   *)
(******************************************************************************)

const
  CROSSING_RANDOM = 0.2;

procedure SetCrossingParents(const AParents: TParentList);

  function _Random: Boolean;
  begin
    Result := Random <= CROSSING_RANDOM;
  end;

  procedure _Crossing(AParent1, AParent2: TParent);
  var
    i, iCount: Integer;
    xV1, xV2: Double;
    xInd: Integer;
  begin
    iCount := AParent1.Gens.Count;
    if _Random then
    begin
      xInd  := Random(iCount);
      for i := 0 to iCount - 1 do
      begin
        if (i >= xInd) then
        begin
          xV1 := AParent1.Gens[i];
          xV2 := AParent2.Gens[i];
          AParent1.Gens[i] := xV2;
          AParent2.Gens[i] := xV1;
        end;
      end;
    end;
  end;

var
  i, iCount: Integer;
  xParent1, xParent2: TParent;
begin
  iCount := AParents.Count;
  if iCount > 0 then
  begin
    i := 0;
    while i < iCount do
    begin
      xParent1 := AParents[i];
      xParent2 := AParents[i + 1];
      _Crossing(xParent1,xParent2);
      Inc(i,2);
    end;
  end;
end;

(******************************************************************************)
(* Реализация алгоритма мутации генов                                         *)
(******************************************************************************)

const
  MUTATION_RANDOM = 0.5;

procedure SetMutationParents(const AParents: TParentList);

  function _Random: Boolean;
  begin
    Result := Random <= MUTATION_RANDOM;
  end;

  procedure _Norm(var AValue1, AValue2: Double);
  var
    xTmp: Double;
  begin
    if AValue1 > AValue2 then
    begin
      xTmp := AValue1;
      AValue2 := AValue1;
      AValue2 := xTmp;
    end;
  end;

  procedure _Mutation(AParent1, AParent2: TParent);
  var
    i, iCount: Integer;
    xP1, xP2, xDelta: Double;
    xV1, xV2: Double;
    xInd: Integer;
  begin
    iCount := AParent1.Gens.Count;
    for i := 0 to iCount - 1 do
    begin
      if _Random then
      begin
        xV1 := AParent1.Gens[i];
        xV2 := AParent2.Gens[i];
        _Norm(xV1,xV2);

        xP1 := xV1 - 0.5 * (xV2 - xV1);
        xP2 := xV2 + 0.5 * (xV2 - xV1);

        xDelta := (xP2 - xP1);
        xV1 := xP1 + Random * xDelta;
        xV2 := xP2 + Random * xDelta;

        AParent1.Gens[i] := xV1;
        AParent2.Gens[i] := xV2;
      end;
    end;
  end;

var
  i, iCount: Integer;
  xParent1, xParent2: TParent;
  xChild1,  xChild2 : TParent;
begin
  iCount := AParents.Count;
  if iCount > 0 then
  begin
    i := 0;
    while i < iCount do
    begin
      xParent1 := AParents[i];
      xParent2 := AParents[i + 1];
      _Mutation(xParent1,xParent2);
      Inc(i,2);
    end;
  end;
end;

{ TGeneration }

constructor TGeneration.Create;
begin
  FTraderMans  := TTraderManList.Create;
  FInfoTraders := TInfoTraderList.Create;
end;

destructor TGeneration.Destroy;
begin
  FreeAndNil(FInfoTraders);
  FreeAndNil(FTraderMans);
  inherited;
end;

function TGeneration.GetCountActiveTrader: Integer;
var
  xTrader: TTraderMan;
  i, iCount, xCnt: Integer;
begin
  xCnt   := 0;
  iCount := TraderMans.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTrader := TraderMans[i];
      if xTrader.Transaction.IsActive then
        Inc(xCnt);
    end;
  Result := xCnt;
end;

function TGeneration.IsAllKill: Boolean;
begin
  Result := GetCountActiveTrader = 0;
end;

procedure TGeneration.SetCreateTrader(const ACountTrader: Integer);
var
  i: Integer;
  xTrader: TTraderMan;
begin
  for i := 0 to ACountTrader - 1 do
  begin
    xTrader := TTraderMan.Create;
    xTrader.ID := i;
    TraderMans.Add(xTrader);
  end;
end;

procedure TGeneration.SerSortResult;
begin
  // Сортируем результат, получаем на иболее успешные
  SetInsetrTraders(TraderMans);
  SetSeleted(InfoTraders);
end;

procedure TGeneration.Calculate(const AIndexBlock: Integer; const ACurrentPrice: Double; const AValues: array of Double);

  procedure _SetTraderMan(const AIndexBlock: Integer; const ACurrentPrice: Double; const ATrader: TTraderMan);
  begin
    if ATrader.Transaction.IsActive then
    begin
      // Проиводим вычисление
      ATrader.Calculate(AValues);

      // Реализуем принятые решение
      case ATrader.TypeDecision of
        tdBuy : ATrader.Transaction.Operation(ACurrentPrice,1,'B');
        tdWait: ATrader.Transaction.OperationWait;
        tdSell: ATrader.Transaction.Operation(ACurrentPrice,1,'S');
      end;

      // Отмечаем возрост рейдера
      ATrader.Age := AIndexBlock;
    end;
  end;

var
  xTrader: TTraderMan;
  i, iCount: Integer;
begin
  iCount := FTraderMans.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTrader := FTraderMans[i];
      _SetTraderMan(AIndexBlock, ACurrentPrice, xTrader);
    end;
end;

{ TParent }

constructor TParent.Create;
begin
  FGens := TGenList.Create;
end;

destructor TParent.Destroy;
begin
  FreeAndNil(FGens);
  inherited;
end;

end.
