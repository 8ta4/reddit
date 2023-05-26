# reddit

## Introduction

- What is this tool all about?
    - This tool is a game-changer for Reddit marketing. It lets you find Reddit posts that talk about a problem that your product can solve. You just need to mark a post from the past and the tool will keep an eye on new posts that are similar to it. When it finds one, it will let you know so you can jump in and show off your product.

- How does it work?
    - It uses some fancy stuff like natural language processing and machine learning to understand what Reddit posts are saying and how they relate to your product. You can mark posts as positive or negative examples to teach the tool what kind of posts you are looking for. The tool will then score the posts based on how similar they are to your examples and notify you when there is a match.

## Installation

- How do you install the tool?
    - The tool is packaged as a Nix package, so you don't have to worry about specifying dependencies yourself. Nix takes care of all that for you. To install the Reddit post monitoring tool, you just need to have Nix installed on your system. If you don't have it yet, you can download and install it from the [Nix download page](https://nixos.org/download.html).

    Once you have Nix installed, you can install the Reddit tool by running the following command in your terminal:
    ```
    $ nix-env -iA TODO
    ```

- What are the benefits of using a Nix package?
    - Using a Nix package for the Reddit post monitoring tool has several benefits. For one, Nix takes care of all dependencies, so you don't have to worry about specifying them yourself. This makes the installation process easier and more reliable.

    Nix also offers atomic upgrades and rollbacks, allowing you to easily switch between different versions of the Reddit post monitoring tool without any issues. This ensures that you always have the latest features and improvements while also giving you the option to revert to a previous version if needed.


## Marking Positive and Negative Examples

- How do you mark a Reddit post as a positive or negative example?
    - To mark a Reddit post as a positive or negative example, you'll need to first create or modify the YAML file. The YAML file should follow this format:

    ```
    {{topic}}:
      training:
        {{URL}}:
          text: |
            {{text}}
          score: {{score}}
      threshold: {{threshold}}
    ```

    Replace `{{topic}}`, `{{URL}}`, `{{text}}`, `{{score}}`, and `{{threshold}}` with the appropriate values for the post you want to mark. For example:

    ```
    example_topic:
      training:
        https://www.reddit.com/r/example/comments/123abc/some_post_title/:
          text: |
            Post content here...
          score: 0.9
      threshold: 0.8
    ```

    Once you've added the entry to the YAML file, save the file and use it as a configuration for the Reddit post monitoring tool. The tool will then use the information from the YAML file to mark the post and train the machine learning model accordingly.

## Sourcing Similar Posts

- How does the tool find similar posts?
    - The tool use a text similarity cosine similarity to compare the text of the positive examples with the text of the new posts.

## Real-time Notifications

- So, how will the tool notify me of similar posts?
    - In the MVP, the tool won't send real-time notifications like email, in-app notifications, or a Chrome extension. Instead, it'll add similar posts to a file. You can set up your own watch tools to keep an eye on this file and notify you when there are changes. That way, you can customize your notifications and still stay in the loop.

- How can I set up a watch tool to keep an eye on the file?
    1. First, pick a file watch tool that works best for you, like [fswatch](https://emcrisostomo.github.io/fswatch/) for macOS or [inotify-tools](https://github.com/inotify-tools/inotify-tools/wiki) for Linux.
    2. Install your chosen file watch tool on your system, following the instructions in its documentation.
    3. Configure the tool to monitor the file where similar posts are added.
    4. Set up a notification method within the watch tool, like sending an email or showing a desktop notification when the file changes.

By doing this, you'll stay updated on similar posts as they appear on the monitored subreddit. And as the tool evolves and more features are added, you can expect even better notification options in the future.
