(******************************************************************************)
(* Вариант до работки приложения, предоставление графика из цены              *)
(******************************************************************************)
unit UnitCustomChartFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  ///<summary>Поток для перерисовки графика</summary>
  TBitmapThread = class(TThread)
  private
    FBitmap: TBitmap;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap;
  end;

  ///<summary>Кастом форма для программирования</summary>
  TCustomChartFrame = class(TFrame)
    Image: TImage;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TBitmapThread }

constructor TBitmapThread.Create;
begin
  inherited Create();
end;

destructor TBitmapThread.Destroy;
begin

  inherited;
end;

{ TCustomChartFrame }

constructor TCustomChartFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TCustomChartFrame.Destroy;
begin

  inherited;
end;

procedure TCustomChartFrame.TimerTimer(Sender: TObject);
begin
  // Постоянная перерисовка графиика
end;



end.
