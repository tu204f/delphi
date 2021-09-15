unit UnitMainForm;

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
  FMX.TreeView,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Menus,
  FMX.Edit,
  FMX.TabControl,
  Lb.SysUtils,
  Lb.Core.Events,
  Lb.ModuleTableFrame,
  Lb.WinFrame,
  Lb.ModuleUserFrame,
  Lb.DomainTableFrame;

type
  TMainForm = class(TForm)
    Layout: TLayout;
    LayoutModule: TLayout;
    LayoutClient: TLayout;
    TabControl: TTabControl;
    TabItemDataBase: TTabItem;
    TabItemDomain: TTabItem;
    LayoutDomain: TLayout;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  protected
    WinFrame: TWinFrame;
    procedure SetInitializationWinFrame;
  protected
    ModuleTableFrame: TModuleTableFrame;
    procedure SetInitializationModule;
  protected
    ModuleUserFrame: TModuleUserFrame;
    procedure SetInitializationModuleUserFrame;
  protected
    DomainTableFrame: TDomainTableFrame;
    procedure SetInitializationDomainTableFrame;
  protected
    procedure SetShowWinFrame(const ATitle: String; const AFrame: TFrame = nil);
    procedure SetCloseWinFrame;
  private
    procedure EventApplyParams(Sender: TObject; const AParams: TStrings);
    procedure EventCloseParams(Sender: TObject; const AParams: TStrings);
    procedure EventModuleAddParams(Sender: TObject; const AParams: TStrings);
    procedure EventModuleChangeParams(Sender: TObject; const AParams: TStrings);
    procedure EventModuleDeleteParams(Sender: TObject; const AParams: TStrings);
    procedure EventModuleDlClickParams(Sender: TObject; const AParams: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.ApplicationVersion,
  Lb.SysUtils.Structure,
  Lb.ModuleFrame,
  Lb.Create.DB;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Собьтие окна
  ApplicationEvents.GetEvents(EVENT_WIN_FRAME_APPLY).EventStrings := EventApplyParams;
  ApplicationEvents.GetEvents(EVENT_WIN_FRAME_CLOSE).EventStrings := EventCloseParams;

  // Событие от модулей
  ApplicationEvents.GetEvents(EVENT_MODULE_ADD).EventStrings := EventModuleAddParams;
  ApplicationEvents.GetEvents(EVENT_MODULE_CHANGE).EventStrings := EventModuleChangeParams;
  ApplicationEvents.GetEvents(EVENT_MODULE_DELETE).EventStrings := EventModuleDeleteParams;
  ApplicationEvents.GetEvents(EVENT_MODULE_TABLE_DLCLICK).EventStrings := EventModuleDlClickParams;


  Self.SetInitializationWinFrame;
  Self.SetInitializationModule;
  Self.SetInitializationModuleUserFrame;
  Self.SetInitializationDomainTableFrame;
end;

destructor TMainForm.Destroy;
begin
  if Assigned(DomainTableFrame) then
    FreeAndNil(DomainTableFrame);

  if Assigned(ModuleTableFrame) then
    FreeAndNil(ModuleTableFrame);

  if Assigned(ModuleUserFrame) then
    FreeAndNil(ModuleUserFrame);

  if Assigned(WinFrame) then
    FreeAndNil(WinFrame);

  inherited;
end;


procedure TMainForm.FormResize(Sender: TObject);
var
  X, Y,xWidth, xHeight: Single;
begin
  if Assigned(WinFrame) then
  begin
    X := 2;
    Y := 2;
    xWidth := Self.Width - 4;
    xHeight := Self.Height  - 4;
    WinFrame.SetBounds(X, Y, xWidth, xHeight);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  xS: String;
begin
  xS := 'Cоздание базы данных (PostgreSQL): ';
  xS := xS + GetApplicationVersion;
  {$IFDEF DEBUG}
  xS := xS + ' debug';
  {$ENDIF}
  Self.Caption := xS;
end;

procedure TMainForm.SetInitializationModule;
begin
  ModuleTableFrame := TModuleTableFrame.Create(nil);
  ModuleTableFrame.Parent := LayoutModule;
  ModuleTableFrame.Align := TAlignLayout.Client;


  Structure.GetTableModules(CrModules);
  ModuleTableFrame.Modules := CrModules;
end;

procedure TMainForm.SetInitializationWinFrame;
begin
  WinFrame := TWinFrame.Create(Self);
  WinFrame.Visible := False;
  WinFrame.Parent := Self;
end;

procedure TMainForm.SetInitializationModuleUserFrame;
begin
  ModuleUserFrame := TModuleUserFrame.Create(Self);
  ModuleUserFrame.Parent := LayoutClient;
  ModuleUserFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.SetInitializationDomainTableFrame;
begin
  DomainTableFrame := TDomainTableFrame.Create(Self);
  DomainTableFrame.Parent := LayoutDomain;
  DomainTableFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.SetShowWinFrame(const ATitle: String; const AFrame: TFrame = nil);
begin
  WinFrame.Title := ATitle;
  WinFrame.Visible := True;
  WinFrame.Win := AFrame;
end;

procedure TMainForm.SetCloseWinFrame;
begin
  WinFrame.Visible := False;
end;

procedure TMainForm.EventApplyParams(Sender: TObject; const AParams: TStrings);
begin
  // -------------------
  Self.SetCloseWinFrame;
end;

procedure TMainForm.EventCloseParams(Sender: TObject; const AParams: TStrings);
begin
  // -------------------
  Self.SetCloseWinFrame;
end;

// ****************************************************************************
// Событие отсписк модулей

procedure TMainForm.EventModuleAddParams(Sender: TObject; const AParams: TStrings);
var
  xModuleFrame: TModuleFrame;
begin
  xModuleFrame := TModuleFrame.Create(nil);
  xModuleFrame.Status := TStatusFrame.fsCreate;
  Self.SetShowWinFrame('Создать новый модуль',xModuleFrame);
end;

procedure TMainForm.EventModuleChangeParams(Sender: TObject; const AParams: TStrings);
var
  xModuleFrame: TModuleFrame;
begin
  xModuleFrame := TModuleFrame.Create(nil);
  xModuleFrame.Status := TStatusFrame.fsChange;
  Self.SetShowWinFrame('Редактировать модель',xModuleFrame)
end;

procedure TMainForm.EventModuleDeleteParams(Sender: TObject; const AParams: TStrings);
begin
  //
end;

procedure TMainForm.EventModuleDlClickParams(Sender: TObject; const AParams: TStrings);
begin

end;

end.
