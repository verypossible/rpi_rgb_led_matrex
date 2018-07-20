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
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  @doc """
  Stop the GenServer and release the Matrix resources.

  Parameters:
   * `pid` the process id of the GenServer that is being terminated
  """
  @spec release(pid) :: :ok
  def release(pid) do
    GenServer.cast(pid, :release)
  end

  @doc """
  Sets one led of the matrix to a color value.
  
  Parameters:
   * `pid` the process id of the GenServer that is being terminated
   * `color` a tuple representing the `r`, `g`, and `b` values of the color to set the pixel
   * `point` a tuple representing the `x` and `y` coordinates of the pixel to be set
  """
  @spec set_pixel(pid, {integer, integer, integer}, {integer, integer}) :: :ok
  def set_pixel(pid, color, point) do
    padded_color = pad_value(color, 3)
    padded_point = pad_value(point, 2)
    data = padded_color ++ padded_point
    GenServer.cast(pid, {:set_pixel, data})
  end

  @doc """
  Fills the whole matrix of leds with the provided color value.

  Parameters:
   * `pid` the process id of the GenServer that is being terminated
   * `color` a tuple representing the `r`, `g`, and `b` values of the color to fill the matrix
  """
  @spec fill(pid, {integer, integer, integer}) :: :ok
  def fill(pid, color) do
    GenServer.cast(pid, {:fill, pad_value(color, 3)})
  end

  @doc """
  Clears the whole matrix of leds.

  Parameters:
   * `pid` the process id of the GenServer that is being terminated
  """
  @spec clear(pid) :: :ok
  def clear(pid) do
    GenServer.cast(pid, :clear)
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
      |> Enum.map(fn(val) -> Integer.to_string(val) |> String.pad_leading(max_length, "0") end)
  end
end

