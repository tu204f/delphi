unit UnitQuikTableGridForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Quik.Manager.DDE,
  Quik.ValueTable;

type
  ///<summary>Информация по выводим таблицам</summary>
  TQuikTableGridForm = class(TForm)
    TabControl: TTabControl;
    StringGrid: TStringGrid;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    FQuikTable: TQuikTable;
  protected
    procedure LoadQuikTables;
    procedure SelectedQuikTable(const AIndex: Integer);
    property QuikTable: TQuikTable read FQuikTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure ShowQuikTableGrid;
  end;

implementation

{$R *.dfm}

var
  localQuikTableGridForm: TQuikTableGridForm = nil;


{ TQuikTableGridForm }

class procedure TQuikTableGridForm.ShowQuikTableGrid;
begin
  if not Assigned(localQuikTableGridForm) then
    localQuikTableGridForm := TQuikTableGridForm.Create(nil);
  localQuikTableGridForm.Show;
end;

constructor TQuikTableGridForm.Create(AOwner: TComponent);
begin
  inherited;
  FQuikTable := nil;
end;

destructor TQuikTableGridForm.Destroy;
begin
  FQuikTable := nil;
  inherited;
end;

procedure TQuikTableGridForm.FormShow(Sender: TObject);
begin
  // Читаем таблицу
  LoadQuikTables;
  SelectedQuikTable(0);
  Timer.Enabled := True;
end;

procedure TQuikTableGridForm.LoadQuikTables;
begin
  // Загружаем наименование таблицы
  QuikManagerTable.SetNameTables(TabControl.Tabs);
end;

procedure TQuikTableGridForm.SelectedQuikTable(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < QuikManagerTable.Tables.Count) then
  begin
    FQuikTable := QuikManagerTable.Tables[AIndex];
    Self.Caption := 'Выбрана таблица: [' + FQuikTable.Name + ']';
  end;
end;


procedure TQuikTableGridForm.TabControlChange(Sender: TObject);
begin
  // Выбираем таблицу, которую нужно выводить
  SelectedQuikTable(TabControl.TabIndex);
end;

procedure TQuikTableGridForm.TimerTimer(Sender: TObject);
var
  xCol, xRow: Integer;
  xColCount, xRowCount: Integer;
begin
  if Assigned(FQuikTable) then
  begin
    xRowCount := FQuikTable.RowCount;
    xColCount := FQuikTable.ColCount;

    if xRowCount > 1 then
    begin
      StringGrid.RowCount := xRowCount;
      StringGrid.ColCount := xColCount;

      for xRow := 0 to xRowCount - 1 do
      begin
        with StringGrid.Rows[xRow] do
        begin
          Clear;
          for xCol := 0 to xColCount - 1 do
            Add(FQuikTable.Cells[xCol,xRow].AsString);
        end;
      end;
    end
    else
    begin
      StringGrid.RowCount := 2;
      StringGrid.Rows[0].Clear;
      StringGrid.Rows[1].Clear;
    end;
  end;
end;

initialization

finalization
  FreeAndNil(localQuikTableGridForm);

end.
