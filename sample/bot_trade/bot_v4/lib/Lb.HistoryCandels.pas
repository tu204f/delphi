unit Lb.HistoryCandels;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils,
  Lb.Candel.StructuresFile;

const
  COUNT_SOURCE_CANDEL = 50;
  COUNT_FUTURE_CANDEL = 20;

type
  TOnEventMemoryCandels = procedure(const ASander: TObject; const AProgress, ACount: Integer) of object;

  ///<summary>Объект по формирование списка структур</summary>
  TFormationStructures = class(TObject)
  private
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnProgress: TNotifyEvent;
    FActive: Boolean;
    FDataCandels: TMemoryStructures;
    FStructures: TStructuresFile;
    procedure SetFileName(const Value: String);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure DoProgress;
    procedure Execution;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property DataCandels: TMemoryStructures read FDataCandels write FDataCandels;
    property Structures: TStructuresFile read FStructures write FStructures;
    property OnStart: TNotifyEvent write FOnStart;
    property OnStop: TNotifyEvent write FOnStop;
    property OnProgress: TNotifyEvent write FOnProgress;
    property FileName: String write SetFileName;
  end;

  THistoryCandels = class(TObject)
  private
    FCandels: TCandelList;
    FIndexBase: Integer;
    // История на основание которой принимается решение
    FDataCandels: TMemoryStructures;
    // История на котороем тестируем и обучаем истемы
    FBaseCandels: TMemoryCandels;
    // Список структру
    FStructures: TStructureList;
    function GetEOF: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFileName(const AFileNameData, AFileNameBase: String);

    procedure First;
    procedure Next;
    property EOF: Boolean read GetEOF;

  end;

implementation

uses
   System.Threading;

function GetMemoryCandelsToCandels(
  const AMemoryCandels: TMemoryCandels;
  const ACandels: TCandelList;
  const AIndex, ACount: Integer): Boolean;
var
  xCandel: TCandel;
begin
  if ACount > 0 then
    for var i := 0 to ACount - 1 do
    begin
      var xInd := i + AIndex;
      if xInd >= AMemoryCandels.Count then
        Break;
      xCandel := AMemoryCandels.Candels[xInd];
      xCandel.Status := TTypeCandel.tcSource;
      ACandels.Add(xCandel);
    end;
  Result := ACandels.Count = ACount;
end;

procedure SetEmptyValueToCandels(
  const ACandels: TCandelList;
  const ACount: Integer);
var
  xCount: Integer;
  xCandel: TCandel;
begin
  // Пустые значение — заполняем пустым набором
  xCount := ACandels.Count;
  if xCount > 0 then
  begin
    xCandel := ACandels[xCount - 1];
    xCandel.Status := TTypeCandel.tcFuture;
    for var i := 0 to ACount - 1 do
    begin
      ACandels.Add(xCandel);
    end;
  end;
end;

{ TFormationStructures }

constructor TFormationStructures.Create;
begin
  FActive := False;
  FDataCandels := TMemoryStructures.Create;
  FStructures := TStructuresFile.Create;
end;

destructor TFormationStructures.Destroy;
begin
  FreeAndNil(FStructures);
  FreeAndNil(FDataCandels);
  inherited;
end;

procedure TFormationStructures.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TFormationStructures.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TFormationStructures.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

procedure TFormationStructures.Execution;
var
  xTack: ITask;
begin
  {todo: ТИпа не не хвататет памяти}
  // Испольнение
  xTack := TTask.Create(
    procedure()
    const
      SIZE_PROGRESS = 100;
    var
      xIndexProgress: Integer;
      xVectorStructure: TVectorStructure;
    begin

      xIndexProgress := 0;
      TThread.Synchronize(nil,DoStart);
      try
        FDataCandels.FirstStructure;
        {todo: Нужно будет контролировать испольнение потока}
        while not FDataCandels.EOF do
        begin
          xVectorStructure := TVectorStructure.Create;
          xVectorStructure.Transform(FDataCandels.Structure);
          FStructures.Add(xVectorStructure);
          FreeAndNil(xVectorStructure);
          if not FActive then
            Break;
          {todo: прогресс сделать в отдельном потоке}
          Inc(xIndexProgress);
          if xIndexProgress >= SIZE_PROGRESS then
          begin
            xIndexProgress := 0;
            TThread.Synchronize(nil,DoProgress);
            //DoProgress;
          end;
          FDataCandels.NextStructure;
        end;
        TThread.Synchronize(nil,DoStop);
      except
        on E : Exception do
          raise Exception.Create('Error Message: TFormationStructures.Execution ' + sLineBreak +  '{' + E.Message + '}');
      end;
    end
  );
  xTack.Start;
end;

procedure TFormationStructures.SetFileName(const Value: String);
begin
  FStructures.FileName := Value;
end;

procedure TFormationStructures.Start;
begin
  if not FActive then
  begin
    FActive := True;
    Execution;
  end;
end;

procedure TFormationStructures.Stop;
begin
  if FActive then
    FActive := False;
end;

{ THistoryCandels }

constructor THistoryCandels.Create;
begin
  FDataCandels := TMemoryStructures.Create;
  FBaseCandels := TMemoryCandels.Create;
  FStructures  := TStructureList.Create;
  FCandels := TCandelList.Create;
end;

destructor THistoryCandels.Destroy;
begin
  FreeAndNil(FCandels);
  FreeAndNil(FStructures);
  FreeAndNil(FBaseCandels);
  FreeAndNil(FDataCandels);
  inherited;
end;

procedure THistoryCandels.LoadFileName(const AFileNameData, AFileNameBase: String);
begin
  FDataCandels.FileName := AFileNameData;
  FBaseCandels.FileName := AFileNameBase;
end;

procedure THistoryCandels.First;
begin

end;

procedure THistoryCandels.Next;
begin
  // Копируем часть свечай для анализа
  GetMemoryCandelsToCandels(
    FBaseCandels,
    FCandels,
    FIndexBase,
    COUNT_SOURCE_CANDEL);

  // По имеющему пакету найти не обходымиы параметры
  // Найти не обходымый


  // Добираем часть свечай - Пустые значение
  SetEmptyValueToCandels(FCandels,COUNT_FUTURE_CANDEL);

  Inc(FIndexBase);
end;

function THistoryCandels.GetEOF: Boolean;
begin
  Result := False;
end;


end.
