let read_problem filename =
  let channel = open_in filename in
  let str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  problem_of_string str