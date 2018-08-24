defmodule RpiRgbLedMatrex.Matrix do
  use GenServer

  @moduledoc """
  This module enables Elixir to communicate with the rpi-rgb-led-matrix lib
  """

  defmodule State do
    @moduledoc false
    defstruct port: nil
  end

  # Public API
  @doc """
  Start and link a Matrix GenServer.

  Parameters:
   * `opts` are any options to pass to GenServer.start_link
  """
  @spec start_link([term]) :: {:ok, pid}
  def start_link(opts \\ [name: __MODULE__]) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @doc """
  Stop the GenServer and release the Matrix resources.
  """
  @spec release() :: :ok
  def release() do
    GenServer.cast(__MODULE__, :release)
  end

  @doc """
  Sets one led of the matrix to a color value.

  Parameters:
   * `color` a tuple representing the `r`, `g`, and `b` values of the color to set the pixel
   * `point` a tuple representing the `x` and `y` coordinates of the pixel to be set
  """
  @spec set_pixel({integer, integer, integer}, {integer, integer}) :: :ok
  def set_pixel(color, point) do
    padded_color = pad_value(color, 3)
    padded_point = pad_value(point, 2)
    data = padded_color ++ padded_point
    GenServer.cast(__MODULE__, {:set_pixel, data})
  end

  @doc """
  Fills the whole matrix of leds with the provided color value.

  Parameters:
   * `color` a tuple representing the `r`, `g`, and `b` values of the color to fill the matrix
  """
  @spec fill({integer, integer, integer}) :: :ok
  def fill(color) do
    GenServer.cast(__MODULE__, {:fill, pad_value(color, 3)})
  end

  @doc """
  Clears the whole matrix of leds.
  """
  @spec clear() :: :ok
  def clear() do
    GenServer.cast(__MODULE__, :clear)
  end

  # gen_server callbacks
  def init(:ok) do
    executable = 'sudo ' ++ :code.priv_dir(:rpi_rgb_led_matrex) ++ '/c/matrix-port'
    port = Port.open({:spawn, executable}, [{:packet, 2}])
    state = %State{port: port}
    {:ok, state}
  end

  def handle_cast({:set_pixel, data}, state) do
    call_port(state.port, '0', data)
    {:noreply, state}
  end

  def handle_cast({:fill, data}, state) do
    call_port(state.port, '1', data)
    {:noreply, state}
  end

  def handle_cast(:clear, state) do
    call_port(state.port, '2')
    {:noreply, state}
  end

  def handle_cast(:release, state) do
    close_port(state.port)
    {:stop, :normal, state}
  end

  # Private helper functions
  defp call_port(port, command, arguments \\ []) do
    # Convert arguments + command into string
    msg = [command | arguments] |> Enum.join()
    Port.command(port, msg)
  end

  defp close_port(port) do
    Port.close(port)
  end

  defp pad_value(values, max_length) do
    Tuple.to_list(values)
    |> Enum.map(fn val -> Integer.to_string(val) |> String.pad_leading(max_length, "0") end)
  end
end
