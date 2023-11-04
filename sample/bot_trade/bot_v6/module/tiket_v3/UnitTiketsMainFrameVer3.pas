(*******************************************************************************
  Применение использование средний скользаящий и оценки коредора
*******************************************************************************)
unit UnitTiketsMainFrameVer3;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.SysUtils.Candel,
  Lb.Module.SysUtils,
  FMX.Objects,
  FMX.Layouts,
  FMX.ListBox, System.Rtti, FMX.Grid.Style, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Grid, FMX.Memo.Types, FMX.Memo, FMX.TabControl;

const
  COUNT_TIKETS = 5000;

type
  TTiketsMainFrameVer3 = class(TFrame, IModule)
    Text: TText;
    TabControl: TTabControl;
    TabItemPrice: TTabItem;
    StringGrid: TStringGrid;
  private
    FTikets: TTiketList;
    FMemoryTikets: TMemoryTikets;
    procedure SetTiket(const ATiket: TTiket);
  protected
    function Start: Boolean;
    function Stop: Boolean;
    function UpData: Boolean;
    function GetCaption: WideString;
    ///<summary>Информация доступа по тикам</summary>
    property MemoryTikets: TMemoryTikets read FMemoryTikets;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.Setting;

{ TTiketsMainFrame }

constructor TTiketsMainFrameVer3.Create(AOwner: TComponent);

const
  SIZE_WIDTH_COLUMN = 50;

  procedure _AddColumn(const AName: String; AStrGrid: TStringGrid; const ASizeColumn: Integer = 150);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AStrGrid);
    xColumn.Header := AName;
    xColumn.Parent := AStrGrid;
    xColumn.Width := ASizeColumn;
  end;

  procedure _SetInitializationColumn;
  var
    i, iCount: Integer;
  begin
    iCount := Trunc(StringGrid.Width / SIZE_WIDTH_COLUMN);
    if iCount > 1 then
      iCount := iCount - 1;
    for i := 0 to iCount - 1 do
      _AddColumn('P' + i.ToString,StringGrid,SIZE_WIDTH_COLUMN);
  end;

begin
  inherited Create(AOwner);
  FMemoryTikets := TMemoryTikets.Create;
  FTikets := TTiketList.Create;

  _SetInitializationColumn;
  StringGrid.RowCount := 0;
end;

destructor TTiketsMainFrameVer3.Destroy;
begin
  FreeAndNil(FTikets);
  FreeAndNil(FMemoryTikets);
  inherited;
end;

function TTiketsMainFrameVer3.GetCaption: WideString;
begin
  Result := 'Торговая система на основе тикетов';
end;

function TTiketsMainFrameVer3.Start: Boolean;
begin
  Result := True;
  FTikets.Clear;

  // Значение работы объекты
  FMemoryTikets.FileName := TSetting.ReadString(CONFIG_FILE_NAME);
  FMemoryTikets.First;
end;

function TTiketsMainFrameVer3.Stop: Boolean;
begin
  Result := True;
end;

function TTiketsMainFrameVer3.UpData: Boolean;
var
  xTiket: TTiket;
begin
  Result := True;
  try
    if FMemoryTikets.EOF then
    begin
      Result := False;
    end
    else
    begin
      xTiket := FMemoryTikets.Tiket;
      // Читаем тикер работы к
      SetTiket(xTiket);
      FMemoryTikets.Next;
    end;
  except
    Result := False;
  end;
end;

procedure TTiketsMainFrameVer3.SetTiket(const ATiket: TTiket);
begin
  // Получаем каждый новый тик
  if FTikets.Count > COUNT_TIKETS then
    FTikets.Delete(0);
  FTikets.Add(ATiket);
  Text.Text := ATiket.ToShortString;
end;


initialization
  RegistrationFrame('TIKETS_VER3',TTiketsMainFrameVer3);

finalization

end.
