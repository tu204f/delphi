unit UnitMainForm;

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
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.ExtCtrls,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable, UnitUserOrderFrame;

type
  ///<summary>Главная форма</summary>
  TMainForm = class(TForm)
    ButtonTolls: TButton;
    EditSecurity: TEdit;
    ButtonQuikTable: TButton;
    Timer: TTimer;
    GridPanel: TGridPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    UserOrderFrame1: TUserOrderFrame;
    UserOrderFrame2: TUserOrderFrame;
    procedure ButtonTollsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonQuikTableClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
  public
    QuikTable: TQuikTable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  UnitQuikTableForm,
  UnitToolsForm,
  Lb.Setting,
  UnitAddOrderForm,
  Lb.SysUtils;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  QuikTable := nil;
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Управление заявками (По RSI)';
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _SetConnetQuikTable;
  var
    xQuikTableName: String;
  begin
    xQuikTableName := TSetting.ReadString('config.sys.quik_table_rsi','');
    if not xQuikTableName.IsEmpty then
    begin
      var xInd := QuikManagerTable.IndexOfTable(xQuikTableName);
      if xInd >= 0 then
        QuikTable := QuikManagerTable.Tables[xInd];
    end;
  end;


var
  xS: String;
begin
  // отслеживаем
  if Assigned(QuikTable) then
  begin
    QuikTable.Fisrt;
    xS := 'QUIK.RSI:' + QuikTable.AsByIndexString(8);
    EditSecurity.Text := xS;
  end else
    _SetConnetQuikTable;
end;

procedure TMainForm.ButtonTollsClick(Sender: TObject);
begin
  // Настройка работы
  ToolsForm.ShowModal;
end;

procedure TMainForm.ButtonQuikTableClick(Sender: TObject);
begin
  QuikTableForm.Show;
end;

end.
