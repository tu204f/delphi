unit UnitHistoryFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  Lb.SysUtils, FMX.Menus;

type
  THistoryFrame = class(TFrame)
    StrGrid: TStringGrid;
    PopupMenu: TPopupMenu;
    MenuItemSaveFileCSV: TMenuItem;
    procedure MenuItemSaveFileCSVClick(Sender: TObject);
  private
    FCandels: TCandelList;
    procedure ShowCandelGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpData(const ACandels: TCandelList);
  end;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  Lb.WorkBot.Grid;

{ THistoryFrame }

constructor THistoryFrame.Create(AOwner: TComponent);

  procedure SetShowStringGridCandel;
  begin
    SetAddColumn(StrGrid,'ID',50);
    SetAddColumn(StrGrid,'Time',150);
    SetAddColumn(StrGrid,'TimeInt',150);
    SetAddColumn(StrGrid,'Open');
    SetAddColumn(StrGrid,'High');
    SetAddColumn(StrGrid,'Low');
    SetAddColumn(StrGrid,'Close');
    SetAddColumn(StrGrid,'Vol');
    SetAddColumn(StrGrid,'color');
  end;

begin
  inherited;
  SetShowStringGridCandel;
end;

destructor THistoryFrame.Destroy;
begin

  inherited;
end;

procedure THistoryFrame.MenuItemSaveFileCSVClick(Sender: TObject);
var
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + 'history.csv';
  FCandels.SaveFileCSV(xFileName);
end;

procedure THistoryFrame.SetUpData(const ACandels: TCandelList);
begin
  FCandels := ACandels;
  ShowCandelGrid;
end;

procedure THistoryFrame.ShowCandelGrid;
var
  xCandel: TCandel;
  i, iCount: Integer;

begin
  // Исторические данные
  iCount := FCandels.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := FCandels[i];
      with StrGrid do
      begin
        Cells[0,i] := (i + 1).ToString;
        Cells[1,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
        Cells[2,i] := xCandel.Time.ToString;
        Cells[3,i] := xCandel.Open.ToString;
        Cells[4,i] := xCandel.High.ToString;
        Cells[5,i] := xCandel.Low.ToString;
        Cells[6,i] := xCandel.Close.ToString;
        Cells[7,i] := xCandel.Vol.ToString;
        Cells[8,i] := GetStrToTypeCandel(xCandel.TypeCandel);
      end;
    end;
end;

end.
