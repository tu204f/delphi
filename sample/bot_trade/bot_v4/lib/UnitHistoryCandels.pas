unit UnitHistoryCandels;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, FMX.Layouts,
  Lb.Candel.SysUtils,
  UnitCandelsFrame,
  FMX.Objects, FMX.ListBox;

const
  COUNT_CANDEL = 50;

type
  THistoryFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Timer: TTimer;
    Text1: TText;
    Text2: TText;
    Layout1: TLayout;
    Layout2: TLayout;
    LayoutResultList: TLayout;
    ListBox: TListBox;
    procedure TimerTimer(Sender: TObject);
  private
    FFileNameData: String;
    FFileNameBase: String;

    FIndexBase: Integer;

    {todo: Заменить его работу с базой - где буду хнить информация отекущем состоние объекта}
    FDataCandels: TMemoryStructures;

    FBaseCandels: TMemoryCandels;

    FStructures: TStructureList;
  private
    FDataCandelsFrame: TCandelsFrame;
    FBaseCandelsFrame: TCandelsFrame;
    procedure SetInitializationFrame;
    function GetActive: Boolean;
    procedure SetIntStructures;
  protected
    procedure SetAnalizCandels(const ACandels: TCandelList; const AStructure: TStructure);
    procedure SetShowCandels;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Next;

    procedure StructureNext;

    procedure LoadFile;

    property FileNameData: String read FFileNameData write FFileNameData;
    property FileNameBase: String read FFileNameBase write FFileNameBase;

    property Active: Boolean read GetActive;
  end;

implementation

{$R *.fmx}

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

{ THistoryFrame }

constructor THistoryFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataCandels := TMemoryStructures.Create;
  FBaseCandels := TMemoryCandels.Create;
  SetInitializationFrame;
  FStructures := TStructureList.Create;
end;

destructor THistoryFrame.Destroy;
begin
  FreeAndNil(FStructures);
  FreeAndNil(FDataCandelsFrame);
  FreeAndNil(FBaseCandelsFrame);
  FreeAndNil(FBaseCandels);
  FreeAndNil(FDataCandels);
  inherited;
end;

procedure THistoryFrame.SetInitializationFrame;
begin
  FDataCandelsFrame := TCandelsFrame.Create(nil);
  FDataCandelsFrame.Parent := Layout2;
  FDataCandelsFrame.Align := TAlignLayout.Client;

  FBaseCandelsFrame := TCandelsFrame.Create(nil);
  FBaseCandelsFrame.Parent := Layout1;
  FBaseCandelsFrame.Align := TAlignLayout.Client;
end;

procedure THistoryFrame.SetAnalizCandels(const ACandels: TCandelList; const AStructure: TStructure);
var
  xPrice, xVol: Double;
  xVectorCandels: TCandelList;
begin
  SetLastPriceAndVol(ACandels, xPrice, xVol);
  xVectorCandels := TCandelList.Create;
  try
    SetTransformCandels(ACandels,xVectorCandels,xPrice, xVol);
  finally
    FreeAndNil(xVectorCandels);
  end;
end;

procedure THistoryFrame.SetShowCandels;
var
  xCandels: TCandelList;
begin

//  xCandels := TCandelList.Create;
//  try
//    GetMemoryCandelsToCandels(FDataCandels,xCandels,FIndexBase,COUNT_CANDEL);
//    FDataCandelsFrame.SetCandels(xCandels);
//  finally
//    FreeAndNil(xCandels);
//  end;

  xCandels := TCandelList.Create;
  try
    // Копируем часть свечай для анализа
    GetMemoryCandelsToCandels(
      FBaseCandels,
      xCandels,
      FIndexBase,
      COUNT_CANDEL);

    // По имеющему пакету найти не обходымиы параметры
    // Найти не обходымый


    // Добираем часть свечай - Пустые значение
    SetEmptyValueToCandels(xCandels,20);


    FBaseCandelsFrame.SetCandels(xCandels);
  finally
    FreeAndNil(xCandels);
  end;

end;

procedure THistoryFrame.StructureNext;
var
  xValueStructire: TValueStructure;
begin
  // Структура
  FDataCandels.NextStructure;
  xValueStructire := FDataCandels.Structure;



end;

procedure THistoryFrame.SetIntStructures;
var
  xValueStructure: TValueStructure;
begin
  FStructures.Clear;
  FDataCandels.FirstStructure;
  while not FDataCandels.EOF do
  begin
    // Реализовать прогресс оперции
    xValueStructure := TValueStructure.Create;
    FStructures.Add(xValueStructure);
    FDataCandels.NextStructure;
  end;
end;

procedure THistoryFrame.Next;
begin
  Inc(FIndexBase);
  Self.SetShowCandels;
end;

procedure THistoryFrame.Start;
begin
  FIndexBase := 0;
  if not Self.Active then
  begin
    // Начать формрование массива объекта
    SetIntStructures;
    //Timer.Enabled := True;
  end;
end;

procedure THistoryFrame.Stop;
begin
  if Self.Active then
    Timer.Enabled := False;
end;



procedure THistoryFrame.TimerTimer(Sender: TObject);
begin
  Inc(FIndexBase);
  Self.SetShowCandels;
end;

function THistoryFrame.GetActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure THistoryFrame.LoadFile;
begin
  FDataCandels.FileName := FFileNameBase;
  FBaseCandels.FileName := FFileNameData;
end;

end.
