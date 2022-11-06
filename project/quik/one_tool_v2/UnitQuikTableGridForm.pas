unit UnitQuikTableGridForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.ListBox,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  System.Generics.Collections,
  Quik.Manager.DDE,
  Quik.ValueTable;

type
  TColumnList = TObjectList<TColumn>;

  TQuikTableGridForm = class(TForm)
    GridPanelLayout: TGridPanelLayout;
    ListBox: TListBox;
    StringGrid: TStringGrid;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ListBoxChange(Sender: TObject);
  private
    FColumns: TColumnList;
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

{$R *.fmx}

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
  inherited Create(AOwner);
  FQuikTable := nil;
  FColumns := TColumnList.Create;
end;

destructor TQuikTableGridForm.Destroy;
begin
  FQuikTable := nil;
  FColumns.Clear;
  FreeAndNil(FColumns);
  inherited;
end;

procedure TQuikTableGridForm.FormShow(Sender: TObject);
begin
  LoadQuikTables;
  SelectedQuikTable(0);
  Timer.Enabled := True;
end;

procedure TQuikTableGridForm.ListBoxChange(Sender: TObject);
begin
  // Выбираем таблицу, которую нужно выводить
  SelectedQuikTable(ListBox.ItemIndex);
end;

procedure TQuikTableGridForm.LoadQuikTables;
begin
  // Загружаем наименование таблицы
  QuikManagerTable.SetNameTables(ListBox.Items);
end;

procedure TQuikTableGridForm.SelectedQuikTable(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < QuikManagerTable.Tables.Count) then
  begin
    FQuikTable := QuikManagerTable.Tables[AIndex];
    Self.Caption := 'Выбрана таблица: [' + FQuikTable.Name + ']';
  end;
end;

procedure TQuikTableGridForm.TimerTimer(Sender: TObject);

  procedure _InitColunum(const AColCount: Integer);
  var
    xColumn:  TStringColumn;
  begin

    if FColumns.Count <> AColCount then
    begin
      FColumns.Clear;
      for var xCol := 0 to AColCount - 1 do
      begin
        xColumn := TStringColumn.Create(nil);
        xColumn.Header := FQuikTable.Cells[xCol,0].AsString;
        xColumn.Parent := StringGrid;
        FColumns.Add(xColumn);
      end;
    end;
  end;

var
  xCol, xRow: Integer;
  xColCount, xRowCount: Integer;
begin
  try
    if Assigned(FQuikTable) then
    begin
      xRowCount := FQuikTable.RowCount;
      xColCount := FQuikTable.ColCount;
      if xRowCount > 1 then
      begin
        StringGrid.RowCount := xRowCount - 1;
        _InitColunum(xColCount);
        for xRow := 1 to xRowCount - 1 do
          for xCol := 0 to xColCount - 1 do
            StringGrid.Cells[xCol,xRow - 1] := FQuikTable.Cells[xCol,xRow].AsString;
      end
      else
        StringGrid.RowCount := 0;
    end;
  except
    Timer.Enabled := False;
    raise Exception.Create('Error Message: Ошибка');
  end;
end;



initialization

finalization
  FreeAndNil(localQuikTableGridForm);

end.
