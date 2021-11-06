unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Поток – который генерирует файл</summary>
  TBaseThread = class(TThread)
  private
    FOnBegin: TNotifyEvent;
    FOnEnd: TNotifyEvent;
  protected
    procedure DoBegin; virtual;
    procedure DoEnd; virtual;
    procedure DoWork; virtual;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property OnBegin: TNotifyEvent write FOnBegin;
    property OnEnd: TNotifyEvent write FOnEnd;
  end;

implementation

{ TBaseThread }

constructor TBaseThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TBaseThread.Destroy;
begin

  inherited;
end;

procedure TBaseThread.DoBegin;
begin
  if Assigned(FOnBegin) then FOnBegin(Self);
end;

procedure TBaseThread.DoEnd;
begin
  if Assigned(FOnEnd) then FOnEnd(Self);
end;

procedure TBaseThread.DoWork;
begin

end;

procedure TBaseThread.Execute;
begin
  try
    Synchronize(DoBegin);
    Self.DoWork;
    Synchronize(DoEnd);
  except
    on E : Exception do
      raise Exception.Create('Error Message: ' + E.Message);
  end;
end;

end.
